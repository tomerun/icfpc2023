require "json"

START_TIME      = Time.utc.to_unix_ms
TL              = (ENV["TL"]? || 2000).to_i
PART            = (ENV["PART"]? || 1).to_i
INITIAL_TEMP    = (ENV["IT"]? || 100).to_f * 1e-5
FINAL_TEMP      = (ENV["FT"]? || 10).to_f * 1e-5
MOVE_DIST       = (ENV["MD"]? || 1000).to_f * 0.1
INF             = 1 << 28
EMPTY           =  -1
BLOCK_BY_PILLAR =  -2
ATT_BUCKET_SIZE = 100
M_BUCKET_SIZE   = 100
COUNTER         = Counter.new
STOPWATCH       = StopWatch.new
RND             = Random.new(2)

class StopWatch
  def initialize
    @start_at = Hash(String, Int64).new
    @sum = Hash(String, Int64).new(0i64)
  end

  def start(name)
    {% if flag?(:local) %}
      @start_at[name] = Time.utc.to_unix_ms
    {% end %}
  end

  def stop(name)
    {% if flag?(:local) %}
      @sum[name] += Time.utc.to_unix_ms - @start_at[name]
    {% end %}
  end

  def to_s(io)
    {% if flag?(:local) %}
      io << @sum
    {% end %}
  end
end

class Counter
  def initialize
    @hist = [] of Int32
  end

  def add(i)
    {% if flag?(:local) %}
      while @hist.size <= i
        @hist << 0
      end
      @hist[i] += 1
    {% end %}
  end

  def to_s(io)
    io << "counter:\n"
    ((@hist.size + 9) // 10).times do |i|
      io << @hist[((i * 10)...(i * 10 + 10))]
      io << "\n"
    end
  end
end

macro debug(msg)
  {% if flag?(:local) %}
    STDERR.puts({{msg}})
  {% end %}
end

macro debugf(format_string, *args)
  {% if flag?(:local) %}
    STDERR.printf({{format_string}}, {{*args}})
  {% end %}
end

def crash(msg, caller_line = __LINE__)
  STDERR.puts "[ERROR] line #{caller_line}: #{msg}"
  exit
end

macro assert(cond, msg = "", caller_line = __LINE__)
  {% if flag?(:local) %}
    if !({{cond}})
      crash({{msg}}, {{caller_line}})
    end
  {% end %}
end

def shuffle(a)
  (a.size - 1).times do |i|
    pos = RND.rand(a.size - i) + i
    a[i], a[pos] = a[pos], a[i]
  end
end

def dist2(y0, x0, y1, x1)
  return (y0 - y1) ** 2 + (x0 - x1) ** 2
end

def dist2(p0, p1)
  return (p0.y - p1.y) ** 2 + (p0.x - p1.x) ** 2
end

#####################
# end of template/lib
#####################

record Pos, y : Float64, x : Float64

record Rect, bottom : Float64, left : Float64, top : Float64, right : Float64

record Pillar, pos : Pos, r : Float64

record Attendee, pos : Pos, taste : Array(Float64)

enum SortType
  M
  A
  P
end

enum ChangeType
  SWAP
  JUMP
  MOVE
end

class Result
  property :score, :ps

  def initialize(@ps : Array(Pos), @score : Float64, @amp : Array(Bool))
  end

  def to_s(io)
    json = {
      "placements" => @ps.map { |p| {"x" => p.x, "y" => p.y} },
      "volumes"    => @amp.map { |am| am ? 10.0 : 0.0 },
    }
    io << json.to_json << "\n"
  end
end

RES_EMPTY = Result.new([] of Pos, -1e10, [] of Bool)

def block(ip0, op, ap, r)
  dy = ap.y - ip0.y
  dx = ap.x - ip0.x
  dy2 = op.y - ip0.y
  dx2 = op.x - ip0.x
  len = dy2 * dy + dx2 * dx
  return false if len <= 0
  norm2 = dy * dy + dx * dx
  len2 = len * len / norm2
  return false if len2 > norm2
  dist2 = dy2 * dy2 + dx2 * dx2 - len2
  return dist2 < r * r
end

def touch_points(p0, p1, r)
  r2 = r * r
  my = p1.y - p0.y
  mx = p1.x - p0.x
  dist2 = my ** 2 + mx ** 2
  dist = dist2 ** 0.5
  len_side2 = dist2 - r * r
  len_side = len_side2 ** 0.5
  cos = len_side / dist
  len_m = len_side * cos
  ratio = len_m / dist
  ey = my * ratio
  ex = mx * ratio
  len_vert = (len_side2 - len_m ** 2) ** 0.5
  ratio_vert = len_vert / len_m
  return [-1, 1].map do |sign|
    ty = ex * ratio_vert * sign
    tx = -ey * ratio_vert * sign
    ny = p0.y + ey + ty
    nx = p0.x + ex + tx
    dot = (ny - p0.y) * (ny - p1.y) + (nx - p0.x) * (nx - p1.x)
    assert(dot.abs < 1e-7, dot)
    Pos.new(ny, nx)
  end
end

def cross(bottom, left, top, right, p0, p1)
  return false if p0.y <= bottom && p1.y <= bottom
  return false if p0.x <= left && p1.x <= left
  return false if top <= p0.y && top <= p1.y
  return false if right <= p0.x && right <= p1.x
  # TODO
  return true
end

def rect_dist2(bottom, left, top, right, p)
  if p.y < bottom
    if p.x < left
      return dist2(p.y, p.x, bottom, left)
    elsif p.x < right
      return (bottom - p.y) ** 2
    else
      return dist2(p.y, p.x, bottom, right)
    end
  elsif p.y < top
    if p.x < left
      return (left - p.x) ** 2
    elsif p.x < right
      return 0.0
    else
      return (p.x - right) ** 2
    end
  else
    if p.x < left
      return dist2(p.y, p.x, top, left)
    elsif p.x < right
      return (p.y - top) ** 2
    else
      return dist2(p.y, p.x, top, right)
    end
  end
end

class Solver
  @instruments : Array(Int32)
  @attendees : Array(Attendee)
  @pillars : Array(Pillar)
  @mn : Int32
  @an : Int32

  def initialize
    prob = JSON.parse(STDIN.gets_to_end).as_h
    room_w = prob["room_width"].as_f
    room_h = prob["room_height"].as_f
    @room = Rect.new(0.0, 0.0, room_h, room_w)
    stage_w = prob["stage_width"].as_f
    stage_h = prob["stage_height"].as_f
    pos = prob["stage_bottom_left"].as_a
    left = pos[0].as_f
    bottom = pos[1].as_f
    @stage = Rect.new(bottom + 10.0, left + 10.0, bottom + stage_h - 10.0, left + stage_w - 10.0)
    musicians = prob["musicians"].as_a
    @instruments = musicians.map { |v| v.as_i }
    @mn = @instruments.size
    @inst_mi = Array(Array(Int32)).new(@instruments.uniq.size) { [] of Int32 }
    @mn.times do |i|
      @inst_mi[@instruments[i]] << i
    end
    @in = @inst_mi.size
    @quality = Array(Float64).new(@mn, 1.0)
    @attendees = prob["attendees"].as_a.map do |a|
      h = a.as_h
      x = h["x"].as_f
      y = h["y"].as_f
      taste = h["tastes"].as_a.map { |v| v.as_f }
      Attendee.new(Pos.new(y, x), taste)
    end
    @an = @attendees.size
    @pillars = prob["pillars"].as_a.map do |p|
      ph = p.as_h
      cx, cy = ph["center"].as_a.map { |v| v.as_f }
      r = ph["radius"].as_f
      Pillar.new(Pos.new(cy, cx), r)
    end
    @blocked_by = Array(Array(Int32)).new(@mn) { Array.new(@an, -1) }
    # blocker_of[i] = [{j, k}] := musician i is blocking sound of musician j to attendee k
    @blocker_of = Array(Set(Tuple(Int32, Int32))).new(@mn) { Set(Tuple(Int32, Int32)).new }
    @raw_score = Array(Float64).new(@mn, 0.0)
    @att_rows = Array(Array(Int32)).new((room_h / ATT_BUCKET_SIZE).floor.to_i + 1) { [] of Int32 }
    @att_cols = Array(Array(Int32)).new((room_w / ATT_BUCKET_SIZE).floor.to_i + 1) { [] of Int32 }
    @orig_taste = [] of Array(Float64)
    @an.times do |ai|
      @att_rows[(@attendees[ai].pos.y / ATT_BUCKET_SIZE).floor.to_i] << ai
      @att_cols[(@attendees[ai].pos.x / ATT_BUCKET_SIZE).floor.to_i] << ai
      @orig_taste << @attendees[ai].taste.dup
      @attendees[ai].taste.map! { |v| {v, 0.0}.max }
    end
    mbh = (stage_h / M_BUCKET_SIZE).floor.to_i + 1
    mbw = (stage_w / M_BUCKET_SIZE).floor.to_i + 1
    @m_buckets = Array(Array(Set(Int32))).new(mbh) { Array.new(mbw) { Set(Int32).new } }
  end

  def mb_pos(pos)
    r = pos.y - @stage.bottom
    c = pos.x - @stage.left
    return {(r / ATT_BUCKET_SIZE).floor.to_i, (c / ATT_BUCKET_SIZE).floor.to_i}
  end

  def solve(timelimit)
    best_res, score = create_initial_solution()
    best_res.score = 0.0
    debug("initial_score:#{best_res.score} pure_score:#{score} #{verify_score(best_res.ps)}")
    STDERR.puts("create_initial_solution:#{Time.utc.to_unix_ms - START_TIME}")

    change_types = [] of ChangeType
    cnt_cand_pos = (((@stage.right - @stage.left) / 10).floor.to_i + 1) * (((@stage.top - @stage.bottom) / (5 * (3 ** 0.5))).floor.to_i + 1)
    {(cnt_cand_pos / @mn).ceil.to_i, 10}.min.times { change_types << ChangeType::MOVE }
    if @in != 1
      20.times { change_types << ChangeType::SWAP }
    end
    initial_jump_prob = {cnt_cand_pos / @mn, 10.0}.min * 0.09
    jump_prob = initial_jump_prob
    debug("cnt_cand_pos:#{cnt_cand_pos} initial_jump_prob:#{initial_jump_prob}")
    mps = best_res.ps.dup
    turn = 0
    initial_cooler = 1.0 / INITIAL_TEMP
    final_cooler = 1.0 / FINAL_TEMP
    cooler = initial_cooler
    begin_time = Time.utc.to_unix_ms
    total_time = timelimit - begin_time
    nega_ratio = 0.0
    while true
      # debug("score:#{score} verify_score:#{verify_score(mps)}")
      # @mn.times do |mi|
      #   mbi = mb_pos(mps[mi])
      #   assert(@m_buckets[mbi[0]][mbi[1]].includes?(mi))
      # end
      # @mn.times do |vmi|
      #   @an.times do |vai|
      #     bi = @blocked_by[vmi][vai]
      #     if bi >= 0
      #       assert(block(mps[vmi], mps[bi], @attendees[vai].pos, 5.001), [vmi, vai, bi])
      #       assert(@blocker_of[bi].includes?({vmi, vai}), [vmi, vai, bi])
      #     end
      #   end
      # end
      # @mn.times do |vmi|
      #   @blocker_of[vmi].each do |t|
      #     vmi2, vai = t
      #     assert(block(mps[vmi2], mps[vmi], @attendees[vai].pos, 5.001), [vmi, vmi2, vai])
      #     assert(@blocked_by[vmi2][vai] == vmi, [vmi, vmi2, vai, @blocked_by[vmi2][vai]])
      #   end
      # end

      if (turn & 0x3FF) == 0
        cur_time = Time.utc.to_unix_ms
        if cur_time >= timelimit
          STDERR.puts("total_turn: #{turn}")
          break
        end
        ratio = (cur_time - begin_time) / total_time
        cooler = Math.exp(Math.log(initial_cooler) * (1.0 - ratio) + Math.log(final_cooler) * ratio)
        jump_prob = initial_jump_prob * (1.0 - ratio)
        if timelimit - cur_time < TL / 40
          change_types = [ChangeType::MOVE]
        end
        nega_ratio = ratio > 0.9 ? 1.0 : ratio / 0.9
        @an.times do |ai|
          @in.times do |ii|
            v = @orig_taste[ai][ii]
            @attendees[ai].taste[ii] = v >= 0 ? v : v * nega_ratio
          end
        end
        @raw_score.fill(0.0)
        score = 0.0
        @mn.times do |mi|
          @an.times do |ai|
            if @blocked_by[mi][ai] == EMPTY
              d2 = dist2(mps[mi], @attendees[ai].pos)
              v = @attendees[ai].taste[@instruments[mi]]
              raw = v / d2
              score += raw * @quality[mi]
              @raw_score[mi] += raw
            end
          end
        end
        # debug(@m_buckets.map { |row| row.map { |v| v.size } }.flatten.sort)
        # debug("score:#{score} verify_score:#{verify_score(mps)}")
      end
      turn += 1
      change_type = change_types[RND.rand(change_types.size)]
      case change_type
      when ChangeType::SWAP
        COUNTER.add(1)
        STOPWATCH.start("swap")
        i0 = RND.rand(@in)
        i1 = RND.rand(@in - 1)
        i1 += 1 if i1 >= i0
        mi0 = @inst_mi[i0][RND.rand(@inst_mi[i0].size)]
        mi1 = @inst_mi[i1][RND.rand(@inst_mi[i1].size)]
        diff = 0.0
        diff -= @raw_score[mi0] * @quality[mi0]
        diff -= @raw_score[mi1] * @quality[mi1]
        new_q0 = 1.0
        new_q1 = 1.0
        if !@pillars.empty?
          @inst_mi[i0].each do |mi|
            next if mi == mi0
            v = 1.0 / (dist2(mps[mi], mps[mi1]) ** 0.5)
            new_q0 += v
            v_old = 1.0 / (dist2(mps[mi], mps[mi0]) ** 0.5)
            diff += (v - v_old) * @raw_score[mi]
          end
          @inst_mi[i1].each do |mi|
            next if mi == mi1
            v = 1.0 / (dist2(mps[mi], mps[mi0]) ** 0.5)
            new_q1 += v
            v_old = 1.0 / (dist2(mps[mi], mps[mi1]) ** 0.5)
            diff += (v - v_old) * @raw_score[mi]
          end
        end
        new_raw0 = 0.0
        new_raw1 = 0.0
        @an.times do |ai|
          ap = @attendees[ai].pos
          if @blocked_by[mi0][ai] == EMPTY
            d2 = dist2(mps[mi0], ap)
            new_raw1 += @attendees[ai].taste[i1] / d2
          end
          if @blocked_by[mi1][ai] == EMPTY
            d2 = dist2(mps[mi1], ap)
            new_raw0 += @attendees[ai].taste[i0] / d2
          end
        end
        diff += new_raw0 * new_q0
        diff += new_raw1 * new_q1
        STOPWATCH.stop("swap")
        if accept(diff, cooler)
          COUNTER.add(diff.abs < 1e-8 ? 3 : 2)
          STOPWATCH.start("swap_accept")
          score += diff
          mps.swap(mi0, mi1)
          mbi0 = mb_pos(mps[mi0])
          mbi1 = mb_pos(mps[mi1])
          if mbi0 != mbi1
            assert(@m_buckets[mbi0[0]][mbi0[1]].includes?(mi1))
            @m_buckets[mbi0[0]][mbi0[1]].delete(mi1)
            @m_buckets[mbi0[0]][mbi0[1]].add(mi0)
            assert(@m_buckets[mbi1[0]][mbi1[1]].includes?(mi0))
            @m_buckets[mbi1[0]][mbi1[1]].delete(mi0)
            @m_buckets[mbi1[0]][mbi1[1]].add(mi1)
          end
          @quality[mi0] = new_q0
          @quality[mi1] = new_q1
          @raw_score[mi0] = new_raw0
          @raw_score[mi1] = new_raw1
          # debug("mi0:#{mi0} score:#{score} verify_score:#{verify_score(mps)} #{change_type}")
          if !@pillars.empty?
            @inst_mi[i0].each do |mi|
              next if mi == mi0
              v = 1.0 / (dist2(mps[mi], mps[mi0]) ** 0.5)
              v_old = 1.0 / (dist2(mps[mi], mps[mi1]) ** 0.5)
              @quality[mi] += v - v_old
            end
            @inst_mi[i1].each do |mi|
              next if mi == mi1
              v = 1.0 / (dist2(mps[mi], mps[mi1]) ** 0.5)
              v_old = 1.0 / (dist2(mps[mi], mps[mi0]) ** 0.5)
              @quality[mi] += v - v_old
            end
          end
          @blocked_by.swap(mi0, mi1)
          @blocker_of.swap(mi0, mi1)
          @an.times do |ai|
            bi = @blocked_by[mi0][ai]
            assert(bi != mi1)
            if bi == mi0
              @blocked_by[mi0][ai] = mi1
              bi = mi1
            end
            if bi >= 0
              assert(@blocker_of[bi].includes?({mi1, ai}))
              if !@blocker_of[bi].includes?({mi0, ai})
                @blocker_of[bi].delete({mi1, ai})
                @blocker_of[bi] << {mi0, ai}
              end
            end
            bi = @blocked_by[mi1][ai]
            assert(bi != mi0)
            if bi == mi1
              @blocked_by[mi1][ai] = mi0
              bi = mi0
            end
            if bi >= 0
              assert(@blocker_of[bi].includes?({mi0, ai}))
              if !@blocker_of[bi].includes?({mi1, ai})
                @blocker_of[bi].delete({mi0, ai})
                @blocker_of[bi] << {mi1, ai}
              end
            end
          end
          @blocker_of[mi0].each do |t|
            bmi, bai = t
            if bmi != mi1
              assert(@blocked_by[bmi][bai] == mi1)
              @blocked_by[bmi][bai] = mi0
            end
          end
          @blocker_of[mi1].each do |t|
            bmi, bai = t
            if bmi != mi0
              assert(@blocked_by[bmi][bai] == mi0)
              @blocked_by[bmi][bai] = mi1
            end
          end
          STOPWATCH.stop("swap_accept")
          amp_score, amp = amplified_score()
          if amp_score > best_res.score && nega_ratio == 1.0
            best_res = Result.new(mps.dup, amp_score, amp)
            debug("score:#{score} amp_score:#{amp_score} at turn:#{turn} #{change_type}")
          end
        end
      when ChangeType::MOVE
        COUNTER.add(11)
        mi0 = RND.rand(@mn)
        STOPWATCH.start("pos")
        if RND.rand < jump_prob
          change_type = ChangeType::JUMP
          ny = RND.rand * (@stage.top - @stage.bottom) + @stage.bottom
          nx = RND.rand * (@stage.right - @stage.left) + @stage.left
          ok = false
          10.times do
            if @mn.times.all? { |mi| mi0 == mi || dist2(ny, nx, mps[mi].y, mps[mi].x) > 10 ** 2 }
              ok = true
              break
            end
            ny = RND.rand * (@stage.top - @stage.bottom) + @stage.bottom
            nx = RND.rand * (@stage.right - @stage.left) + @stage.left
          end
          if !ok
            STOPWATCH.stop("pos")
            next
          end
        else
          ny = (mps[mi0].y + RND.rand * MOVE_DIST - MOVE_DIST * 0.5).clamp(@stage.bottom, @stage.top)
          nx = (mps[mi0].x + RND.rand * MOVE_DIST - MOVE_DIST * 0.5).clamp(@stage.left, @stage.right)
          ok = false
          10.times do
            if @mn.times.all? { |mi| mi0 == mi || dist2(ny, nx, mps[mi].y, mps[mi].x) > 10 ** 2 }
              ok = true
              break
            end
            ny = (mps[mi0].y + RND.rand * MOVE_DIST - MOVE_DIST * 0.5).clamp(@stage.bottom, @stage.top)
            nx = (mps[mi0].x + RND.rand * MOVE_DIST - MOVE_DIST * 0.5).clamp(@stage.left, @stage.right)
          end
          if !ok
            STOPWATCH.stop("pos")
            next
          end
        end
        STOPWATCH.stop("pos")
        np = Pos.new(ny, nx)
        mp = mps[mi0]
        mps[mi0] = np
        inst = @instruments[mi0]
        diff = 0.0
        diff -= @raw_score[mi0] * @quality[mi0]
        new_quality = @quality.dup
        new_quality[mi0] = 1.0
        new_raw = @raw_score.dup
        new_raw[mi0] = 0.0
        if !@pillars.empty?
          @inst_mi[inst].each do |mi|
            next if mi0 == mi
            v = 1.0 / (dist2(mps[mi], np) ** 0.5)
            new_quality[mi0] += v
            v_old = 1.0 / (dist2(mps[mi], mp) ** 0.5)
            diff += (v - v_old) * @raw_score[mi]
            new_quality[mi] += v - v_old
          end
        end
        STOPWATCH.start("angle")
        new_blocked_by = Array.new(@an, EMPTY)
        # 新しく置いた位置でのブロック関係
        angles = rad_sort(mps, mi0) do |ai, blocker|
          if blocker == EMPTY
            d2 = dist2(np, @attendees[ai].pos)
            v = @attendees[ai].taste[inst] / d2
            new_raw[mi0] += v
          else
            new_blocked_by[ai] = blocker
          end
        end
        diff += new_raw[mi0] * new_quality[mi0]
        STOPWATCH.stop("angle")

        # 動かしたことで届くようになった
        # debug("blocker_size:#{@blocker_of[mi0].size}")
        STOPWATCH.start("new_sort")
        near_mis = (0...@mn).to_a.sort_by { |i| dist2(mps[i], mp) }
        STOPWATCH.stop("new_sort")
        STOPWATCH.start("new")
        @blocker_of[mi0].each do |mi, ai|
          next if block(mps[mi], np, @attendees[ai].pos, 5.0)
          is_blocked = false
          mbi = mb_pos(mps[mi])
          @m_buckets.size.times do |ri|
            bb = @stage.bottom + M_BUCKET_SIZE * ri - 5
            bt = @stage.bottom + M_BUCKET_SIZE * (ri + 1) + 5
            @m_buckets[0].size.times do |ci|
              next if @m_buckets[ri][ci].empty?
              bl = @stage.left + M_BUCKET_SIZE * ci - 5
              br = @stage.left + M_BUCKET_SIZE * (ci + 1) + 5
              if cross(bb, bl, bt, br, mps[mi], @attendees[ai].pos)
                is_blocked = @m_buckets[ri][ci].any? { |bmi| bmi != mi && block(mps[mi], mps[bmi], @attendees[ai].pos, 5.0) }
              end
              break if is_blocked
            end
            break if is_blocked
          end
          next if is_blocked
          # is_blocked = near_mis.any? { |bmi| bmi != mi && block(mps[mi], mps[bmi], @attendees[ai].pos, 5.0) }
          is_blocked |= @pillars.any? { |pl| block(mps[mi], pl.pos, @attendees[ai].pos, pl.r) }
          if !is_blocked
            d2 = dist2(mps[mi], @attendees[ai].pos)
            v = @attendees[ai].taste[@instruments[mi]] / d2
            diff += v * new_quality[mi]
            new_raw[mi] += v
          end
        end
        STOPWATCH.stop("new")
        # 動かしたことで遮られた
        STOPWATCH.start("shut")
        @mn.times do |mi|
          next if mi == mi0
          min_yb, max_yb, min_xb, max_xb = get_att_bucket(np, mps[mi])
          if (max_yb - min_yb + 1) / @att_rows.size < (max_xb - min_xb + 1) / @att_cols.size
            min_yb.upto(max_yb) do |yb|
              @att_rows[yb].each do |ai|
                next if @blocked_by[mi][ai] != EMPTY
                if block(mps[mi], np, @attendees[ai].pos, 5.0)
                  d2 = dist2(mps[mi], @attendees[ai].pos)
                  v = @attendees[ai].taste[@instruments[mi]] / d2
                  diff -= v * new_quality[mi]
                  new_raw[mi] -= v
                end
              end
            end
          else
            min_xb.upto(max_xb) do |xb|
              @att_cols[xb].each do |ai|
                next if @blocked_by[mi][ai] != EMPTY
                if block(mps[mi], np, @attendees[ai].pos, 5.0)
                  d2 = dist2(mps[mi], @attendees[ai].pos)
                  v = @attendees[ai].taste[@instruments[mi]] / d2
                  diff -= v * new_quality[mi]
                  new_raw[mi] -= v
                end
              end
            end
          end
        end
        STOPWATCH.stop("shut")
        if accept(diff, cooler)
          COUNTER.add(diff.abs < 1e-8 ? 13 : 12)
          score += diff
          @quality = new_quality
          @raw_score = new_raw
          mbi = mb_pos(np)
          ombi = mb_pos(mp)
          if ombi != mbi
            assert(!@m_buckets[mbi[0]][mbi[1]].includes?(mi0))
            @m_buckets[mbi[0]][mbi[1]].add(mi0)
            assert(@m_buckets[ombi[0]][ombi[1]].includes?(mi0))
            @m_buckets[ombi[0]][ombi[1]].delete(mi0)
          end

          # debug("mi0:#{mi0} score:#{score} verify_score:#{verify_score(mps)} #{change_type}")

          # 他の人たちへの影響
          STOPWATCH.start("after1")
          blocker0 = @blocker_of[mi0].dup
          @blocker_of[mi0].clear
          blocker0.each do |t|
            mi, ai = t
            assert(@blocked_by[mi][ai] == mi0)
            min_dist = 1e10
            block_by = EMPTY
            mbi = mb_pos(mps[mi])
            @m_buckets.size.times do |ri|
              bb = @stage.bottom + M_BUCKET_SIZE * ri - 5
              bt = @stage.bottom + M_BUCKET_SIZE * (ri + 1) + 5
              @m_buckets[0].size.times do |ci|
                next if @m_buckets[ri][ci].empty?
                bl = @stage.left + M_BUCKET_SIZE * ci - 5
                br = @stage.left + M_BUCKET_SIZE * (ci + 1) + 5
                if min_dist != 1e10 && rect_dist2(bb, bl, bt, br, mps[mi]) > min_dist
                  COUNTER.add(6)
                  next
                end
                if cross(bb, bl, bt, br, mps[mi], @attendees[ai].pos)
                  @m_buckets[ri][ci].each do |bmi|
                    next if mi == bmi
                    if block(mps[mi], mps[bmi], @attendees[ai].pos, 5.0)
                      d2 = dist2(mps[mi], mps[bmi])
                      if d2 < min_dist
                        min_dist = d2
                        block_by = bmi
                      end
                    end
                  end
                end
              end
            end
            if block_by == EMPTY
              @pillars.each do |pl|
                if block(mps[mi], pl.pos, @attendees[ai].pos, pl.r)
                  d2 = dist2(mps[mi], pl.pos)
                  if d2 < min_dist
                    min_dist = d2
                    block_by = BLOCK_BY_PILLAR
                    break
                  end
                end
              end
            end
            @blocked_by[mi][ai] = block_by
            if block_by >= 0
              @blocker_of[block_by] << {mi, ai}
            end
          end
          STOPWATCH.stop("after1")

          STOPWATCH.start("after2")
          @mn.times do |mi|
            next if mi == mi0
            nd2 = dist2(mps[mi], np)
            min_yb, max_yb, min_xb, max_xb = get_att_bucket(np, mps[mi])
            if (max_yb - min_yb + 1) / @att_rows.size < (max_xb - min_xb + 1) / @att_cols.size
              min_yb.upto(max_yb) do |yb|
                @att_rows[yb].each do |ai|
                  if block(mps[mi], np, @attendees[ai].pos, 5.0)
                    if @blocked_by[mi][ai] == EMPTY
                      cd2 = 1e10
                    elsif @blocked_by[mi][ai] == BLOCK_BY_PILLAR
                      cd2 = 1e10
                    else
                      cd2 = dist2(mps[mi], mps[@blocked_by[mi][ai]])
                    end
                    if nd2 < cd2
                      if @blocked_by[mi][ai] >= 0
                        assert(@blocker_of[@blocked_by[mi][ai]].includes?({mi, ai}))
                        @blocker_of[@blocked_by[mi][ai]].delete({mi, ai})
                      end
                      @blocked_by[mi][ai] = mi0
                      @blocker_of[mi0] << {mi, ai}
                    end
                  end
                end
              end
            else
              min_xb.upto(max_xb) do |xb|
                @att_cols[xb].each do |ai|
                  if block(mps[mi], np, @attendees[ai].pos, 5.0)
                    if @blocked_by[mi][ai] == EMPTY
                      cd2 = 1e10
                    elsif @blocked_by[mi][ai] == BLOCK_BY_PILLAR
                      cd2 = 1e10
                    else
                      cd2 = dist2(mps[mi], mps[@blocked_by[mi][ai]])
                    end
                    if nd2 < cd2
                      if @blocked_by[mi][ai] >= 0
                        assert(@blocker_of[@blocked_by[mi][ai]].includes?({mi, ai}))
                        @blocker_of[@blocked_by[mi][ai]].delete({mi, ai})
                      end
                      @blocked_by[mi][ai] = mi0
                      @blocker_of[mi0] << {mi, ai}
                    end
                  end
                end
              end
            end
          end
          STOPWATCH.stop("after2")

          # 移動先からの線のブロック
          STOPWATCH.start("moved")
          @an.times do |ai|
            if @blocked_by[mi0][ai] >= 0
              blockers = @blocker_of[@blocked_by[mi0][ai]]
              assert(blockers.includes?({mi0, ai}))
              blockers.delete({mi0, ai})
            end
            if new_blocked_by[ai] >= 0
              @blocker_of[new_blocked_by[ai]] << {mi0, ai}
            end
          end
          @blocked_by[mi0] = new_blocked_by
          STOPWATCH.stop("moved")

          amp_score, amp = amplified_score()
          if amp_score > best_res.score && nega_ratio == 1.0
            best_res = Result.new(mps.dup, amp_score, amp)
            debug("score:#{score} amp_score:#{amp_score} at turn:#{turn} #{change_type}")
          end

          # @mn.times do |vmi|
          #   @an.times do |vai|
          #     bi = @blocked_by[vmi][vai]
          #     if bi >= 0
          #       assert(block(mps[vmi], mps[bi], @attendees[vai].pos, 5.001), [vmi, vai, bi])
          #       assert(@blocker_of[bi].includes?({vmi, vai}), [vmi, vai, bi])
          #     elsif bi == EMPTY
          #       assert(!@blocker_of[mi0].includes?({vmi, vai}), [vmi, vai, mi0])
          #       assert(!block(mps[vmi], mps[mi0], @attendees[vai].pos, 4.999), [vmi, vai, mi0])
          #     end
          #   end
          # end
          # @mn.times do |vmi|
          #   @blocker_of[vmi].each do |t|
          #     vmi2, vai = t
          #     assert(block(mps[vmi2], mps[vmi], @attendees[vai].pos, 5.001), [vmi, vmi2, vai])
          #     assert(@blocked_by[vmi2][vai] == vmi, [vmi, vmi2, vai, @blocked_by[vmi2][vai]])
          #   end
          # end
        else
          mps[mi0] = mp
        end
      end
    end

    return best_res
  end

  def get_att_bucket(mp, mp2)
    if mp.y <= mp2.y
      min_yb = 0
      max_yb = mp.y + 5 > mp2.y ? @att_rows.size - 1 : ((mp.y + 5) / ATT_BUCKET_SIZE).floor.to_i
    else
      max_yb = @att_rows.size - 1
      min_yb = mp.y - 5 < mp2.y ? 0 : ((mp.y - 5) / ATT_BUCKET_SIZE).floor.to_i
    end
    if mp.x <= mp2.x
      min_xb = 0
      max_xb = mp.x + 5 > mp2.x ? @att_cols.size - 1 : ((mp.x + 5) / ATT_BUCKET_SIZE).floor.to_i
    else
      max_xb = @att_cols.size - 1
      min_xb = mp.x - 5 < mp2.x ? 0 : ((mp.x - 5) / ATT_BUCKET_SIZE).floor.to_i
    end
    return {min_yb, max_yb, min_xb, max_xb}
  end

  def amplified_score
    score = 0.0
    amp = Array.new(@mn, false)
    @mn.times do |i|
      amp[i] = @raw_score[i] >= 0
      if amp[i]
        score += @raw_score[i] * @quality[i] * 10.0
      end
    end
    return score, amp
  end

  def create_initial_solution
    @blocked_by.each { |a| a.fill(-1) }
    @blocker_of.each { |a| a.clear }
    @quality.fill(1.0)
    @raw_score.fill(0.0)
    cand_pos = [] of Pos
    eps = 1e-5
    y = @stage.bottom
    row = ((@stage.top - @stage.bottom) / (5 * (3 ** 0.5) + eps)).floor.to_i + 1
    step_y = row == 1 ? 100.0 : (@stage.top - @stage.bottom) / (row - 1)
    x = @stage.left
    step_x = 10.0
    while y <= @stage.top + 1e-8
      if y > @stage.top - step_y
        y = @stage.top
      end
      x.step(to: @stage.right, by: step_x) do |cx|
        cand_pos << Pos.new(y, cx)
      end
      x = x == @stage.left ? @stage.left + step_x * 0.5 : @stage.left
      y += step_y
    end
    pos_i = cand_pos.size.times.to_a
    used_pos_i = [] of Int32
    max_trial = 50
    @mn.times do |mi|
      if pos_i.size > max_trial
        max_trial.times do |i|
          chi = RND.rand(pos_i.size - i) + i
          pos_i.swap(i, chi)
        end
      end
      inst = @instruments[mi]
      best_i = -1
      best_v = -1e10
      {pos_i.size, max_trial}.min.times do |i|
        pos = cand_pos[pos_i[i]]
        sum = 0.0
        @an.times do |ai|
          d2 = dist2(pos, @attendees[ai].pos)
          v = @attendees[ai].taste[inst]
          sum += v / d2
        end
        if sum > best_v
          best_v = sum
          best_i = i
        end
      end
      pos_i.swap(best_i, -1)
      used_pos_i << pos_i.pop
    end
    mps = used_pos_i.map { |pi| cand_pos[pi] }
    @mn.times do |i|
      mp = mps[i]
      mbi = mb_pos(mp)
      @m_buckets[mbi[0]][mbi[1]] << i
      inst = @instruments[i]
      angles = rad_sort(mps, i) do |ai, blocker|
        if blocker == EMPTY
          # do nothing
          # if pillar_on.all? { |pi| !block(mp, @pillars[pi].pos, @attendees[ang[1]].pos, @pillars[pi].r) }
          #   # not blocked
          #   # debug({i, ang[1], v})
          # else
          #   debug("block_by_pillar #{near_dist} #{dist2(mp, @attendees[ang[1]].pos)}")
          #   @blocked_by[i][ang[1]] = BLOCK_BY_PILLAR
          # end
        elsif blocker == BLOCK_BY_PILLAR
          @blocked_by[i][ai] = BLOCK_BY_PILLAR
        else
          @blocked_by[i][ai] = blocker
          @blocker_of[blocker] << {i, ai}
        end
      end
    end
    if !@pillars.empty?
      @mn.times do |i|
        @inst_mi[@instruments[i]].each do |oi|
          next if i >= oi
          q = 1.0 / (dist2(mps[i], mps[oi]) ** 0.5)
          @quality[i] += q
          @quality[oi] += q
        end
      end
    end
    pure_score = 0.0
    @mn.times do |i|
      @an.times do |j|
        next if @blocked_by[i][j] != EMPTY
        d2 = dist2(mps[i], @attendees[j].pos)
        v = @attendees[j].taste[@instruments[i]]
        raw = v / d2
        @raw_score[i] += raw
      end
      pure_score += @raw_score[i] * @quality[i]
    end
    score, amp = amplified_score()
    return Result.new(mps, score, amp), pure_score
  end

  def rad_sort(mps, mi)
    down = [] of Tuple(SortType, Int32)
    up = [] of Tuple(SortType, Int32)
    left = [] of Tuple(Float64, SortType, Int32)
    right = [] of Tuple(Float64, SortType, Int32)
    mp = mps[mi]
    @an.times do |i|
      dy = @attendees[i].pos.y - mp.y
      dx = @attendees[i].pos.x - mp.x
      if dx == 0
        (dy < 0 ? down : up) << {SortType::A, i}
      elsif dx < 0
        left << {dy / dx, SortType::A, i}
      else
        right << {dy / dx, SortType::A, i}
      end
    end
    @pillars.size.times do |i|
      ps = touch_points(mp, @pillars[i].pos, @pillars[i].r)
      ps.each do |p|
        ty = p.y - mp.y
        tx = p.x - mp.x
        if tx.abs < 1e-8
          if ty > 0
            up << {SortType::P, i}
          else
            if @pillars[i].pos.x == mp.x + @pillars[i].r
              right << {-Float64::INFINITY, SortType::P, i}
            end
          end
        elsif tx < 0
          left << {ty / tx, SortType::P, i}
        else
          right << {ty / tx, SortType::P, i}
        end
      end
    end
    near_mi = nil
    near_dist = 1e10
    @mn.times do |i|
      next if i == mi
      ps = touch_points(mp, mps[i], 5.0)
      ps.each do |p|
        ty = p.y - mp.y
        tx = p.x - mp.x
        if tx.abs < 1e-8
          if ty > 0
            up << {SortType::M, i}
          else
            if mps[i].x == mp.x + 5
              right << {-Float64::INFINITY, SortType::M, i}
            end
          end
        elsif tx < 0
          left << {ty / tx, SortType::M, i}
        else
          right << {ty / tx, SortType::M, i}
        end
      end
    end
    angles = down.dup
    angles += right.sort.map { |v| {v[1], v[2]} }
    angles += up
    angles += left.sort.map { |v| {v[1], v[2]} }
    others_on = Set(Int32).new
    near_mi = nil
    near_dist = 1e10
    @mn.times do |omi|
      next if mi == omi
      my = mps[omi].y - mp.y
      mx = mps[omi].x - mp.x
      if my < 0 && mx.abs < 5
        others_on << omi
        d2 = my ** 2 + mx ** 2
        if d2 < near_dist
          near_dist = d2
          near_mi = omi
        end
      end
    end
    pillar_on = Set(Int32).new
    @pillars.size.times do |pi|
      my = @pillars[pi].pos.y - mp.y
      mx = @pillars[pi].pos.x - mp.x
      if my < 0 && mx.abs < @pillars[pi].r
        pillar_on << pi
        d2 = my ** 2 + mx ** 2
        if d2 < near_dist
          near_dist = d2
        end
      end
    end
    angles.each do |ang|
      case ang[0]
      when SortType::M
        cmi = ang[1]
        d2 = dist2(mp, mps[cmi])
        if others_on.includes?(cmi)
          others_on.delete(cmi)
          if others_on.empty?
            near_mi = nil
            near_dist = 1e10
            pillar_on.each do |pi|
              pd2 = dist2(mp, @pillars[pi].pos)
              if pd2 < near_dist
                near_dist = pd2
              end
            end
          elsif (d2 - near_dist).abs < 1e-9
            near_dist = 1e10
            others_on.each do |omi|
              md = dist2(mp, mps[omi])
              if md < near_dist
                near_dist = md
                near_mi = omi
              end
            end
          end
        else
          others_on << cmi
          if d2 < near_dist
            near_dist = d2
            near_mi = cmi
          end
        end
      when SortType::A
        if near_dist > dist2(mp, @attendees[ang[1]].pos)
          yield ang[1], EMPTY
        else
          if near_mi.nil?
            yield ang[1], BLOCK_BY_PILLAR
          else
            yield ang[1], near_mi
          end
        end
      when SortType::P
        d2 = dist2(mp, @pillars[ang[1]].pos)
        if pillar_on.includes?(ang[1])
          pillar_on.delete(ang[1])
          if (d2 - near_dist).abs < 1e-4
            near_dist = 1e10
            pillar_on.each do |pi|
              pd2 = dist2(mp, @pillars[pi].pos)
              if pd2 < near_dist
                near_dist = pd2
              end
            end
          end
        else
          pillar_on << ang[1]
          near_dist = {near_dist, d2}.min
        end
      end
    end
    # return angles
  end

  def accept(diff, cooler)
    return true if diff >= 0
    # return false
    v = diff * cooler
    return false if v < -8
    return RND.rand < Math.exp(v)
  end

  def verify_score(mps)
    qual = Array.new(@mn, 1.0)
    if !@pillars.empty?
      @mn.times do |i|
        @mn.times do |j|
          next if j == i || @instruments[i] != @instruments[j]
          d = dist2(mps[i], mps[j]) ** 0.5
          qual[i] += 1.0 / d
        end
      end
    end
    sum = 0.0
    @mn.times do |i|
      inst = @instruments[i]
      raw = 0.0
      @an.times do |j|
        a = @attendees[j]
        ap = a.pos
        if @mn.times.all? { |mi| mi == i || !block(mps[i], mps[mi], ap, 5.0) } &&
           @pillars.all? { |pl| !block(mps[i], pl.pos, ap, pl.r) }
          d2 = dist2(mps[i], ap)
          v = a.taste[inst]
          sum += v / d2 * qual[i]
          raw += v / d2
        end
      end
      # debug([i, @raw_score[i], raw])
    end
    return sum
  end
end

def main
  solver = Solver.new
  best_res = RES_EMPTY
  PART.times do |i|
    res = solver.solve(START_TIME + TL * (i + 1) // PART)
    debug([res.score, solver.verify_score(res.ps)])
    if res.score > best_res.score
      best_res = res
    end
  end
  puts best_res
  debug(COUNTER)
  debug(STOPWATCH)
  STDERR.puts("final_score:#{best_res.score}")
end

main
