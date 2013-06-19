# encoding: UTF-8

# libsmaybe I should be using ocaml, but ruby does so well .. :/
#
# BUGS:
# You cannot generate a Makefile from this yet
# A dependency tree should be created first, then this should be turned into
# rake tasks
#
# ./*.ml depending on libraries - this dependenc information is still missisng

# CONFIGURATION: {{{1
# ==================
ACTIVE_BACKENDS = %w{neko}
# additional flags:
CFLAGS = ""
CFLAGS_BACKEND = []

# utils {{{1
# ==========
def system_raise(cmd)
  system(cmd)
  raise "#{cmd} failed" if $? != 0
end


class Ocamlopt
  attr_reader :includes, :preprocessors, :compiler
  attr_writer :compiler

  def initialize()
    # $CFLAGS="#{CFLAGS} -pp camlp4o -g #{$OCAML_INCLUDES}"
    @compiler = "ocamlopt"
    @includes = %w{libs/extlib libs/extc libs/neko libs/javalib libs/ziplib libs/swflib libs/xml-light libs/ttflib}
    @preprocessors = []
  end

  def compile_cmd(source)
    includes = @includes.map {|v| "-I #{v}"}.join(" ")
    preprocessors = @preprocessors.join(' ')
    cflags_backend = "-pp \"cppo #{CFLAGS_BACKEND.join(' ')}\""
    "#{@compiler} #{cflags_backend} #{$ml_extra_flags[source]} #{preprocessors} #{CFLAGS} #{includes} -c #{source}"
  end
end

# use drake instead of rake?
class MultiFile < Rake::FileTask
  private
  def invoke_prerequisites(task_args, invocation_chain) # :nodoc:
    invoke_prerequisites_concurrently(task_args, invocation_chain)
  end
end

def multifile(*args, &block)
    MultiFile.define_task(*args, &block)
end

$ocamlopt = Ocamlopt.new

# implementation start {{{1
# =====================

# getting ml file dependencies from official Makefile - yes this sucks - but is likely to work:

deps_hash = Hash.new
File.open("Makefile","r").lines.each {|line|
  case line
  when /([^.]*)\.cmx: (.*)/
    deps_hash[$1] = $2.split(" ").map {|v| v.gsub(/.cmx$/, '')}
  end
}

deps_hash["ast"] = []

$ml_extra_flags = Hash.new
$ml_extra_flags["parser.ml"] = "-pp camlp4o"

backend_files = Hash.new
backend_files[:swf] = %w{genswf8 genswf9 genswf}
backend_files[:neko] = %w{genneko}
backend_files[:as3] = %w{genas3}
backend_files[:cpp] = %w{gencpp}
backend_files[:cs] = %w{gencs}
backend_files[:java] = %w{genjava}
backend_files[:js] = %w{genjs}
backend_files[:php] = %w{genphp}
# backend_files[:xml] = %w{genxml}

$libs = Hash.new

class Lib
  attr_reader :name, :make_args, :targets, :needs
  def initialize(name, targets)
    @name = name
    @make_args = []
    @targets = targets
    @needs = []
    $libs[name] = self
  end

  def add_make_args(*args); @make_args += args; self end
  def add_lib_dependencies(*args); @needs += args; self end

  def main_target; @main_target ||= "libs/#{@name}/#{@targets[0]}"; @main_target end
  def prerequisites; @needs.map {|v| $libs[v].main_target } end
end
Lib.new(:extlib, %w{extLib.cmxa}).add_make_args("opt")
Lib.new(:extc, %w{extc.cmxa extc_stubs.o}).add_make_args("native").add_lib_dependencies(:extlib)
Lib.new(:neko, %w{neko.cmxa})
Lib.new(:javalib, %w{java.cmxa}).add_lib_dependencies(:extlib)
Lib.new(:ziplib, %w{zip.cmxa}).add_lib_dependencies(:extc)
Lib.new(:swflib, %w{swflib.cmxa})
Lib.new("xml-light".to_sym, %w{xml-light.cmxa}).add_make_args("xml-light.cmxa")
Lib.new(:ttflib, %w{ttf.cmxa}).add_lib_dependencies(:swflib, :extc)

# drop non local files
local_ml_files = Dir["*.ml"]
deps_hash.each_pair do |k,v|
  v.select! {|file| local_ml_files.include? "#{file}.ml" }
end


ACTIVE_BACKENDS.map! {|v| v.to_sym}

# drop unused files:
backend_files.each_pair do |k,v|
  if (ACTIVE_BACKENDS.include? k)
    CFLAGS_BACKEND << "-D BACKEND_#{k}"
  else
    v.each do |file_to_delete|
      deps_hash.delete file_to_delete
      deps_hash.each_pair {|k,v| v.select! {|file| file != file_to_delete }}
    end
  end
end

deps = []
deps_hash.each_pair {|k,v| v.each {|dep| deps << k; deps << dep } }
require 'rgl/adjacency'
dg = RGL::DirectedAdjacencyGraph.__send__(:[], *deps )

# rake tasks {{{1
# ===============

task :help do
puts <<-EOF
usage:
building haxe is done in 2 steps:
1) build libs
2) build haxe


task about libs:
================
clean_libs : run make clean in all library directories
libs : checkout libs directory using git
javalib: complie this library
libs/javalib/java.cmxa: compile javalib

haxe related tasks:
haxe: compile  haxe
clean: clean haxe


checkout tasks: libs

build tasks: haxe compile_libs

most .cmx files and library targets can be compiled directly, eg
EOF
end

# cleaning {{{2
def delete_files(*files)
  files.each {|v|
    File.delete v if File.exist? v
  }
end

task :clean do
  # should this clean everything?
  raise "there is no :clean target. Try clean_haxe, clean_libs, clean_all"
end

task :clean_haxe do
  files = Dir["*.cmx", "*.cmi", "*.annot", "*.o"]
  delete_files(*files)
end

task :clean_graph do
  delete_files("graph.jpg","graph.dot")
end

task :clean_all => [:clean_haxe, :clean_libs, :clean_grap] {}

# dependency graph of ./*.ml files: {{{2

file "graph.jpg" do
  # Use DOT to visualize this graph:
  require 'rgl/dot'
  dg.write_to_graphic_file('jpg')
end

# haxe and its files {{{2

# libs {{{3
file "libs" do
  begin
    sh 'git clone git://github.com/HaxeFoundation/ocamllibs.git libs'
  rescue
    puts "checking out libs failed, rm -fr it to retry" if Dir.exists? "libs"
  end
end

lib_dependencies = $libs.each_pair {|k,v| v.targets}.flatten(1)

$libs.each_pair do|path, lib|

  multifile lib.main_target => ["libs"] + lib.prerequisites do
    make_args = lib.make_args.map {|v| " #{v}"}.join('')
    sh "make -C libs/#{path}#{make_args}"
  end
  # for each alternative target create a new task:
  lib.targets.drop(1).each do |t|
    multifile "libs/#{path}/#{t}" => lib.main_target
  end

  multitask :compile_libs => lib.main_target
  multitask lib.name.to_sym => lib.main_target
end

task :clean_libs do
  $libs.each_pair do|path,opts|
    sh "make -C libs/#{path} clean"
  end
end

# haxe {{{3

# define rake tasks for building haxe, one task for each file
# When changing the setup things may break ..
deps_hash.each_pair {|k,v|
  file "#{k}.cmx" => ["#{k}.ml"] +v.map {|v| "#{v}.cmx"} do
    sh $ocamlopt.compile_cmd("#{k}.ml")
    # ocamlopt -annot -g -I libs/extlib -I libs/extc -I libs/neko -I libs/javalib -I libs/ziplib -I libs/swflib -I libs/xml-light -I libs/ttflib -c interp.ml
  end
}

haxe_local_deps = []
require "rgl/traversal"
dg.depth_first_visit("main") {|n|
  haxe_local_deps << "#{n}.cmx"
}
haxe_local_deps

multifile "haxe" => ["libs/"] + haxe_local_deps do
  # TODO
  libs = []
  libs << "-cclib"
  libs << "libs/extc/extc_stubs.o"
  libs << "-cclib"
  libs << "-lz"
  libs << "unix.cmxa"
  libs << "str.cmxa"
  libs << "libs/extlib/extLib.cmxa"
  libs << "libs/xml-light/xml-light.cmxa"
  libs << "libs/swflib/swflib.cmxa"
  libs << "libs/extc/extc.cmxa"
  libs << "libs/neko/neko.cmxa"
  libs << "libs/javalib/java.cmxa"
  libs << "libs/ziplib/zip.cmxa"
  libs << "libs/ttflib/ttf.cmxa"
  sh "ocamlopt #{libs.join(' ')} -o haxe #{haxe_local_deps.join(' ')}"
end

task :default => []  do
  puts "default task: do nothing"
end
