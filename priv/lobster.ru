require 'lobster'
require 'cowboy_rack'

use Rack::Lint
use Rack::ShowExceptions
# run Rack::Lobster.new
Rack::Handler::CowboyRack.run(Rack::Lobster.new)
