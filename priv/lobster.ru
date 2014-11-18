require '/home/dev/Work/cowboy_rack/priv/lobster'
require '/home/dev/Work/cowboy_rack/priv/cowboy_rack'

use Rack::Lint
use Rack::ShowExceptions
run Rack::Lobster.new
#Rack::Handler::CowboyRack.run(Rack::Lobster.new)
