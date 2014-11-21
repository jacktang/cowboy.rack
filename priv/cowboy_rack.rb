require 'rubygems'
if File.exists?("Gemfile")
  require 'bundler'
  Bundler.setup
end
# Dir.chdir(ARGV[0])
Dir.chdir(ARGV[0][0, ARGV[0].index("/config.ru")])
require 'erlectricity'
require 'stringio'
require 'rack'
module Rack
  module Handler
    class CowboyRack

      def self.run(app, options={})
        receive do |f|
          f.when([:env, Array]) do |headers|
            env = {}
            headers.each do |header|
              env[header[0]] = header[1]
            end
            rack_input = StringIO.new(headers.to_s) 
            rack_input.set_encoding(Encoding::BINARY) if rack_input.respond_to?(:set_encoding)
            env.update({"rack.version" => Rack::VERSION,
               "rack.input" => rack_input,
               "rack.errors" => $stderr,
               "rack.multithread" => false,
               "rack.multiprocess" => false,
               "rack.run_once" => false,
               "rack.url_scheme" => ["yes", "on", "1"].include?(ENV["HTTPS"]) ? "https" : "http"
             })
            status, headers, body = app.call(env)
     
            f.send!([:response, [
              
              [:status, status], 
              [:headers, headers.to_a], 
              [:body, build_body(body)]
            ]])
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
  