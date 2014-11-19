require 'rubygems'
if File.exists?("Gemfile")
  require 'bundler'
  Bundler.setup
end

Dir.chdir(ARGV[0])


require 'erlectricity'
require 'stringio'
require 'rack'
module Rack
  module Handler
    class CowboyRack

      def self.run(app, options={})
        receive do |f|
          f.when(:env, Object) do |env|
            env = env.inject({}){|env_hash, kv| env_hash[kv[0]] = kv[1].to_s; env_hash } 
              # body_len = INPUT.read(4).unpack("N")[0]
              # INPUT = IO.new(3)
              # rack_input = StringIO.new(INPUT.read(body_len))
              # rack_input.set_encoding(Encoding::BINARY) if rack_input.respond_to?(:set_encoding)

            env.update({"rack.version" => Rack::VERSION,
               # "rack.input" => rack_input,
               "rack.errors" => $stderr,
               "rack.multithread" => false,
               "rack.multiprocess" => false,
               "rack.run_once" => false,
               "rack.url_scheme" => ["yes", "on", "1"].include?(ENV["HTTPS"]) ? "https" : "http"
             })
            status, headers, body = app.call(env)
            # f.send!(:response, env.inspect)
            f.send!(:response, [
              [:status, status], 
              [:headers, headers.to_a], 
              [:body, build_body(body)]
            ])
            exit
            # f.receive_loop
          end
        end
      end
      
      private
      def self.build_body(body)
        body_str = ""
        body.each{|str| body_str << str}
        body_str
      end

    end
  end
end
  