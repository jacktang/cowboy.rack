#!/usr/bin/env ruby

Dir.chdir(ARGV[0])

require 'rubygems'
require 'stringio'

if File.exists?("Gemfile")
  require 'bundler'
  Bundler.setup
end

require 'rack'

INPUT = IO.new(3)
OUTPUT = IO.new(4)

def read_request
  env = {}
  
  size = INPUT.read(4)
  if !size || size.size != 4
    exit(0)
  end
  size = size.unpack("N")[0]
  count = INPUT.read(4).unpack("N")[0]
  count.times do
    len = INPUT.read(4).unpack("N")[0]
    key = INPUT.read(len)
    len = INPUT.read(4).unpack("N")[0]
    value = INPUT.read(len)
    env[key] = value
  end
  body_len = INPUT.read(4).unpack("N")[0]
  rack_input = StringIO.new(INPUT.read(body_len))
  rack_input.set_encoding(Encoding::BINARY) if rack_input.respond_to?(:set_encoding)
  env.update({"rack.version" => Rack::VERSION,
               "rack.input" => rack_input,
               "rack.errors" => $stderr,
               "rack.multithread" => false,
               "rack.multiprocess" => false,
               "rack.run_once" => false,
               "HTTP_HOST" => env["HTTP_X_FORWARDED-HOST"],
               "rack.url_scheme" => ["yes", "on", "1"].include?(ENV["HTTPS"]) ? "https" : "http"
             })
  if env["HTTP_CONTENT_TYPE"] && env["HTTP_CONTENT_LENGTH"]
      env.update({
                 "CONTENT_TYPE" => env["HTTP_CONTENT_TYPE"],
                 "CONTENT_LENGTH" => env["HTTP_CONTENT_LENGTH"]

        })
      env.delete("HTTP_CONTENT_LENGTH")
      env.delete("HTTP_CONTENT_TYPE")
  end
  env
end

def handle_request(app, env)

  status, headers, body = app.call(env)

  
  packed_body = if body.respond_to?(:to_path) # file
    [1, body.to_path.bytesize, body.to_path].pack("CNa*")
  else
    ary = []
  
    body.each do |s|
      ary << s
    end
  
    t = ary.join("")
  
    [0, t.bytesize, t].pack("CNa*")
  end
  
  body.close if body.respond_to?(:close)
  

  packed = [status, headers.length].pack("NN") + 
           headers.map {|key,value| [key.bytesize, key, value.to_s.bytesize, value.to_s].pack("Na*Na*")}.join("") + 
           packed_body
  
  OUTPUT.write([packed.bytesize].pack("N"))
  OUTPUT.write(packed)
  OUTPUT.flush
end


def load_app
  app, options = Rack::Builder.parse_file("config.ru")
  last_mtime = File.mtime("config.ru")
  app = app.call if app.respond_to?(:arity) && app.arity == 0

  [app, last_mtime]
end

app, last_mtime = load_app

loop do
  env = read_request
  if !app || !last_mtime
    ENV["RAILS_RELATIVE_URL_ROOT"] = env["SCRIPT_NAME"]
    app, last_mtime = load_app
  end
  
  if !app || !last_mtime || File.mtime("config.ru") > last_mtime
    app, last_mtime = load_app
  end
  handle_request(app, env)
end
