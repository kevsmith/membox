require 'rubygems'
require 'redis'

redis = Redis.new({:host => '192.168.1.101'})
requests_processed = 100000

value = 1

(1..requests_processed).each do |i|
  redis.set(i, value)
end

(1..requests_processed).each do |i|
  redis.get i
end

t1 = Time.now
(1..requests_processed).each do |i|
  redis.set(i, value)
end
t2 = Time.now
time = t2 - t1

puts "#{t2.strftime("%H:%M")} [Processed #{requests_processed} writes in #{time} seconds - #{(requests_processed/time).round} requests/sec"

t1 = Time.now
(1..requests_processed).each do |i|
  redis.get i
end
t2 = Time.now
time = t2 - t1

puts "#{t2.strftime("%H:%M")} [Processed #{requests_processed} reads in #{time} seconds - #{(requests_processed/time).round} requests/sec"
