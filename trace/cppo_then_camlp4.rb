#!/usr/bin/env ruby
# encoding: UTF-8
require 'tempfile'

def err(m)
  STDERR.write("#{m}\n")
end

err(ARGV.inspect)

# why do I have to write such crap?

i = ARGV.find_index("--")
cppo_flags = ARGV.take(i)
camlp4_flags = ARGV.drop(i+1)

# the last argument (.ml file) must be passed ot cppo first!
file = camlp4_flags.delete_at(-1)
cppo_flags << file

def sh(cmd)
  r = `#{cmd}`
  raise "#{cmd} failed" unless $? == 0
  r
end

# tmp = Tempfile.new('cppo_result').path
tmp = File.join(File.dirname(file), "tmp_#{File.basename(file)}")
begin
  sh "#{cppo_flags.join(' ')} > #{tmp}"
  puts sh("#{camlp4_flags.join(' ')} #{tmp}")

  # create a symlink to the orignial file so that error locations are fine
  # yes - this sucks
  # File.delete tmp
  # File.symlink(file, tmp)
end
