require 'rubygems'
require 'redis'

redis = Redis.new
requests_processed = 100000

t1 = Time.now
(1..requests_processed).each do |i|
  redis.set(i, i)
end
t2 = Time.now
time = t2 - t1

puts "#{t2.strftime("%H:%M")} [Processed 100000 requests in #{time} seconds - #{(requests_processed/time).round} requests/sec"
