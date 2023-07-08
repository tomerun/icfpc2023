require "json"

START_TIME      = Time.utc.to_unix_ms
TL              = (ENV["TL"]? || 2000).to_i
PART            = (ENV["PART"]? || 1).to_i
INITIAL_COOLER  = (ENV["IC"]? || 3).to_f * 0.0001
FINAL_COOLER    = (ENV["FC"]? || 100).to_f * 0.0001
INF             = 1 << 28
EMPTY           = -1
BLOCK_BY_PILLAR = -2
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
    while @hist.size <= i
      @hist << 0
    end
    @hist[i] += 1
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

class Result
  getter :score, :ps

  def initialize(@ps : Array(Pos), @score : Float64)
  end

  def to_s(io)
    json = {"placements" => @ps.map { |p| {"x" => p.x, "y" => p.y} }}
    io << json.to_json << "\n"
  end
end

RES_EMPTY = Result.new([] of Pos, -1e10)

def block(ip0, ip1, ap)
  dy = ap.y - ip0.y
  dx = ap.x - ip0.x
  dy2 = ip1.y - ip0.y
  dx2 = ip1.x - ip0.x
  len = dy2 * dy + dx2 * dx
  return false if len <= 0
  norm2 = dy * dy + dx * dx
  dist2 = dy2 * dy2 + dx2 * dx2 - len ** 2 / norm2
  return dist2 <= 25.0
end

def touch_points(p0, p1, r)
  r2 = r * r
  my = p1.y - p0.y
  mx = p1.x - p0.x
  dist2 = my ** 2 + mx ** 2
  div = 1.0 / dist2
  b2ac = {(r2 * my) ** 2 - (r2 ** 2 - r2 * (mx ** 2)) * dist2, 0.0}.max ** 0.5
  ret = [] of Pos
  if mx == 0
    dy = (-r2 * my) * div
    ty = p1.y + dy - p0.y
    dx_b = (r2 - dy ** 2) ** 0.5
    {-1, 1}.each do |sign|
      dx = dx_b * sign
      ret << Pos.new(p1.y + dy, p1.x + dx)
    end
  else
    {-1, 1}.each do |sign|
      dy = (-r2 * my + b2ac * sign) * div
      dx = (r2 - dy ** 2) ** 0.5
      ty = p1.y + dy - p0.y
      tx = p1.x + dx - p0.x
      if (ty * dy + tx * dx).abs > 1e-8
        dx *= -1
      end
      ret << Pos.new(p1.y + dy, p1.x + dx)
    end
  end
  assert(ret.size == 2, ret.size)
  ret.each do |p|
    assert(((p.y - p0.y) * (p.y - p1.y) + (p.x - p0.x) * (p.x - p1.x)).abs < 1e-8)
  end
  return ret
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
    @attendees = prob["attendees"].as_a.map do |a|
      h = a.as_h
      x = h["x"].as_f
      y = h["y"].as_f
      taste = h["tastes"].as_a.map { |v| v.as_f }
      Attendee.new(Pos.new(y, x), taste)
    end
    @pillars = prob["pillars"].as_a.map do |p|
      ph = p.as_h
      cx, cy = ph["center"].as_a.map { |v| v.as_f }
      r = ph["radius"].as_f
      Pillar.new(Pos.new(cy, cx), r)
    end
    @mn = @instruments.size
    @an = @attendees.size
    @blocked_by = Array(Array(Int32)).new(@mn) { Array.new(@an, -1) }
    @blocker_of = Array(Array(Tuple(Int32, Int32))).new(@mn) { [] of Tuple(Int32, Int32) }
  end

  def solve(timelimit)
    @blocked_by.each { |a| a.fill(-1) }
    @blocker_of.each { |a| a.clear }

    best_res = create_initial_solution()
    mps = best_res.ps.dup

    turn = 0
    cooler = INITIAL_COOLER
    begin_time = Time.utc.to_unix_ms
    total_time = timelimit - begin_time
    while true
      if (turn & 0xF) == 0
        cur_time = Time.utc.to_unix_ms
        if cur_time >= timelimit
          debug("total_turn: #{turn}")
          break
        end
        ratio = (cur_time - begin_time) / total_time
        cooler = Math.exp(Math.log(INITIAL_COOLER) * (1.0 - ratio) + Math.log(FINAL_COOLER) * ratio)
      end
      turn += 1
    end
    # while true
    #   @ps = orig_ps.dup
    #   res = solve_one(cur_res)
    #   if accept(res.score - cur_res.score, cooler)
    #     COUNTER.add(0)
    #     if res.score > best_res.score
    #       COUNTER.add(1)
    #       debug("best_score:#{res.score} turn:#{turn}")
    #       best_res = res
    #       last_update_turn = turn
    #     end
    #     cur_res = res
    #   end
    #   turn += 1
    # end
    return best_res
  end

  def create_initial_solution
    cand_pos = [] of Pos
    eps = 1e-5
    y = @stage.bottom
    step_y = 10 * (3 ** 0.5) + eps
    x = @stage.left
    while y <= @stage.top
      if y > @stage.top - step_y
        y = @stage.top
      end
      x.step(to: @stage.right, by: 20.0) do |cx|
        cand_pos << Pos.new(y, cx)
      end
      x += x == @stage.left ? 5 : -5
      y += step_y
    end
    pos_i = cand_pos.size.times.to_a
    used_pos_i = [] of Int32
    max_trial = 500
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
    all_sum = 0.0
    mps = used_pos_i.map { |pi| cand_pos[pi] }
    @mn.times do |i|
      mp = mps[i]
      inst = @instruments[i]
      angles = rad_sort(mps, i)
      on = Set(Int32).new
      near_mi = nil
      near_dist = 1e10
      @mn.times do |mi|
        next if i == mi
        my = mps[mi].y - mp.y
        mx = mps[mi].x - mp.x
        if my < 0 && mx.abs < 5
          on << mi
          d2 = my ** 2 + mx ** 2
          if d2 < near_dist
            near_dist = d2
            near_mi = mi
          end
        end
      end
      angles.each do |ang|
        case ang[0]
        when SortType::M
          mi = ang[1]
          d2 = dist2(mp, mps[mi])
          if on.includes?(mi)
            on.delete(mi)
            if on.empty?
              near_mi = nil
              near_dist = 1e10
            elsif (d2 - near_dist).abs < 1e-9
              near_dist = 1e10
              on.each do |omi|
                md = dist2(mp, mps[omi])
                if md < near_dist
                  near_dist = md
                  near_mi = omi
                end
              end
            end
          else
            on << mi
            if d2 < near_dist
              near_dist = d2
              near_mi = mi
            end
          end
        when SortType::A
          if near_mi.nil?
            d2 = dist2(mp, @attendees[ang[1]].pos)
            v = @attendees[ang[1]].taste[inst]
            all_sum += v / d2
            # debug({i, ang[1], v})
          else
            @blocked_by[i][ang[1]] = near_mi
            @blocker_of[near_mi] << {i, ang[1]}
          end
        when SortType::P
          @blocked_by[i][ang[1]] = BLOCK_BY_PILLAR
        end
      end
    end
    return Result.new(mps, all_sum)
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
      # dy = @pillars[i].pos.y - mp.y
      # dx = @pillars[i].pos.x - mp.x
      # if dx == 0
      #   (dy < 0 ? down : up) << {SortType::A, i}
      # elsif dx < 0
      #   left << {dy / dx, SortType::A, i}
      # else
      #   right << {dy / dx, SortType::A, i}
      # end
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
    return angles
  end

  def accept(diff, cooler)
    return true if diff >= 0
    v = diff * cooler
    return false if v < -8
    return RND.next_double < Math.exp(v)
  end

  def verify_score(mps)
    sum = 0.0
    @mn.times do |i|
      inst = @instruments[i]
      @an.times do |j|
        a = @attendees[j]
        ap = a.pos
        if @mn.times.all? { |mi| mi == i || !block(mps[i], mps[mi], ap) }
          d2 = dist2(mps[i], ap)
          v = a.taste[inst]
          sum += v / d2
        end
      end
    end
    return sum
  end
end

def main
  solver = Solver.new
  best_res = RES_EMPTY
  PART.times do |i|
    res = solver.solve(START_TIME + TL * (i + 1) // PART)
    if res.score > best_res.score
      best_res = res
    end
  end
  puts best_res
  debug("final_score:#{best_res.score}")
  # debug(STOPWATCH)
  # debug(COUNTER)
end

main
