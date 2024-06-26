require "http/client"
require "json"
require "option_parser"

ENDPOINT = "api.icfpcontest.com"
TOKEN    = ENV["TOKEN"]
enum SubCommand
  Nop
  Problems
  Submit
  View
end

cmd = SubCommand::Nop
min_id = 0
max_id = 0
limit = 0
offset = 1
dir = ""
parser = OptionParser.new do |parser|
  parser.on("problem", "get problems") do
    cmd = SubCommand::Problems
    dir = "../problem"
    parser.on("-b NUM", "start problem ID") { |v| min_id = v.to_i }
    parser.on("-e NUM", "end problem ID") { |v| max_id = v.to_i }
    parser.on("--dir DIR", "save directory") { |v| dir = v }
  end
  parser.on("submit", "make submissions") do
    cmd = SubCommand::Submit
    dir = "../answer"
    parser.on("-b NUM", "start problem ID") { |v| min_id = v.to_i }
    parser.on("-e NUM", "end problem ID") { |v| max_id = v.to_i }
    parser.on("--dir DIR", "solution file directory") { |v| dir = v }
  end
  parser.on("view", "view submissions") do
    cmd = SubCommand::View
    parser.on("--limit NUM", "limit count") { |v| limit = v.to_i }
    parser.on("--offset NUM", "offset num") { |v| offset = v.to_i }
  end
end

parser.parse

case cmd
when SubCommand::Problems
  problems(min_id, max_id, dir)
when SubCommand::Submit
  submit(min_id, max_id, dir)
when SubCommand::View
  view(limit, offset)
else
  puts parser
end

def problems(min_id, max_id, dir)
  client = HTTP::Client.new(ENDPOINT, tls: true)
  min_id.upto(max_id) do |prob_id|
    res = client.get("/problem?problem_id=#{prob_id}")
    json = JSON.parse(res.body)["Success"].as_s
    File.open("#{dir}/#{sprintf("%04d", prob_id)}.json", "w") do |f|
      f << json
    end
  end
end

def submit(min_id, max_id, dir)
  client = HTTP::Client.new(ENDPOINT, tls: true)
  header = HTTP::Headers{"Authorization" => "Bearer #{TOKEN}", "Content-Type" => "application/json"}
  min_id.upto(max_id) do |prob_id|
    sol = File.read("#{dir}/#{sprintf("%04d", prob_id)}.json")
    body = {"problem_id": prob_id, "contents": sol}
    res = client.post("/submission", headers: header, body: body.to_json)
    puts "prob_id:#{prob_id} submission_id:#{res.body}"
  end
end

def view(limit, offset)
  client = HTTP::Client.new(ENDPOINT, tls: true)
  header = HTTP::Headers{"Authorization" => "Bearer #{TOKEN}"}
  res = client.get("/submissions?offset=#{offset}&limit=#{limit}", headers: header)
  json = JSON.parse(res.body)
  puts json
end
