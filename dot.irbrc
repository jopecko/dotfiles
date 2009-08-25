# -*-ruby-*-

require 'irb/completion'
require 'rubygems'
require 'yaml'
require 'pp'

# start wirble (with color)
begin
  require 'wirble'
  Wirble.init
  # Wirble.colorize unless IRB.conf[:PROMPT_MODE] == :INF_RUBY
rescue LoadError
end

def profile
  t = Time.now
  yield
  "Took #{Time.now - t} seconds."
end

# Inspecting really long strings causes inf-ruby to get really, really slow.
# class String
#   def inspect
#     puts self
#   end
# end

IRB.conf[:AUTO_INDENT]  = true

IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"

IRB.conf[:PROMPT_MODE] = :SIMPLE

