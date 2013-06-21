# encoding: UTF-8
#
# I recommend using drake. rake has several problems

# libsmaybe I should be using ocaml, but ruby does so well .. :/
#
# things to improve:
# * libs/* are built automatically, and dependencies between libraries are set below.
#   However the libraries itself are not built parall yet - add rake tasks and
#   ignore Makefiles?
# * You cannot generate a Makefile from this yet
#
# * ./*.ml depending on libraries - this dependenc information can still be incomplete
#   If you find a problem, fix it :)
#
# Now that generating makefiles is supported I could have used haxe ? what
# about rgl depth_first_visit implementation?

# CONFIGURATION: {{{1
# ==================
require 'rgl/adjacency'
require "rgl/traversal"
require "set"

ALL_BACKENDS= [:neko, :as3, :cpp, :cs, :js, :php, :java, :swf]

# eg set to [:neko] only:
ACTIVE_BACKENDS = ALL_BACKENDS

# additional flags:
ANNOT = "-dtypes" # "-dtypes" or "-annot" or ""

DERIVING_PATH = "deriving"
DERIVING_SUPPORT = false

CPPO_PATH="cppo/cppo"

DEBUGGING_SUPPORT="-g" # "-g" or ""

FLAVOURS = ["native", "bytecode"]

ACTIVE_FLAVOURS = FLAVOURS

# utils {{{1
# ==========
INCLUDES=%w{libs/extlib libs/extc libs/neko libs/javalib libs/ziplib libs/swflib libs/xml-light libs/ttflib}
CFLAGS_BACKEND = []
EXTS_TO_CLEAN = ["*.cmx", "*.cmi", "*.cmo", "*.cma", "*.cmxa", "*.annot", "*.o","*.a"]

class String
  # bad style
  def ocaml_native(native)
    if native == "native"
      self.gsub(".LEXT", ".cmxa").gsub(".CEXT", ".cmx").gsub("OCAML", "ocamlopt")
    else
      self.gsub(".LEXT", ".cma").gsub(".CEXT", ".cmo").gsub("OCAML", "ocamlc")
    end
  end

  def assert_ocaml_native()
    raise "native or bytecode expected" unless %w{native bytecode}.include? self
    self
  end
end

class Array
  # bad style
  def ocaml_native(native)
    self.map {|v| v.ocaml_native(native)}
  end
end

class Symbol
  def ocaml_native(native)
    self
  end
end

class Hash
  # bad style
  def ocaml_native(native)
    h = Hash.new
    self.each_pair {|k,v| h[k.ocaml_native(native)] = v.ocaml_native(native) }
    h
  end
end

$tasks = []

# provides makefile implementation for My_FileTask My_Task
class My_MakeFileTask
  def makefile(out)
    hash = case @hash
    when Hash; @hash
    when String; { @hash => [] }
    end
    raise "unexpected" if hash.keys.length > 1
    hash.each_pair {|k,v|
      out.write(<<-EOF)
#{k.ocaml_native(@native)}: #{v.ocaml_native(@native).join(' ')}
#{@cmds.map {|v| "\t#{v}"}.join("\n")}
      EOF
    }
  end
end

# a File task. target is a file or directory
class My_FileTask < My_MakeFileTask
  include Rake::DSL

  # hash is target => deps
  def initialize(native, hash, cmds)
    @native = native
    @hash = hash
    @cmds = cmds
    $tasks << self
  end

  def rake_task
    my_multifile @native, @hash do
      @cmds.each {|v| sh v}
    end
  end

end

# a "phony" task, target file/dir will not exist (used for aliasing task names)
class My_Task < My_MakeFileTask
  include Rake::DSL

  def initialize(hash, cmds)
    @hash = hash
    @cmds = cmds
    $tasks << self
  end

  def rake_task
    my_multitask @hash do
      @cmds.each {|v| sh v}
    end
  end

end

class My_CleanTask
  include Rake::DSL

  def initialize(name, dir, files = [])
    @name = name
    @files = files
    @dir = dir
    $tasks << self
  end

  def rake_task
    task @name do
      clean_dir @dir
      # TODO clean files
    end
  end

  def makefile(out)
    out.write("#{@name}:\n")
    out.write("\tcd #{@dir}; rm #{EXTS_TO_CLEAN.join(' ')} #{@files.join(' ')}\n")
  end
end

# module Rake
#   class Task
#     def multi
#       self.instance_eval {
#         def invoke_prerequisites(a,b)
#           puts "HERE"
#           invoke_prerequisites_concurrently(a,b)
#         end
#       }
#     end
#   end
# end

class MultiFile < Rake::FileTask
  private
  # rake
  # def invoke_prerequisites(task_args, invocation_chain) # :nodoc:
  #   invoke_prerequisites_concurrently(task_args, invocation_chain)
  # end

  # drake:
  def invoke_prerequisites(args, invocation_chain)
    threads = @prerequisites.collect { |p|
      Thread.new(p) { |r| application[r, @scope].invoke_with_call_chain(args, invocation_chain) }
    }
    threads.each { |t| t.join }
  end

end

def my_multifile(native, hash = {}, *args, &block)
    native.assert_ocaml_native()
    # file(*args, &block)
    MultiFile.define_task(hash.ocaml_native(native), *args, &block)
end

def my_multitask(hash = {}, *args, &block)
    multitask(hash, *args, &block)
end

def system_raise(cmd)
  system(cmd)
  raise "#{cmd} failed" if $? != 0
end

$deps_hash = Hash.new
class Ml
  attr_accessor :extra_flags, :cmx_deps_no_ext

  def initialize(name, cmx_deps_no_ext)
    @name = name
    @cmx_deps_no_ext = cmx_deps_no_ext
    @includes = %w{libs/extlib libs/extc libs/neko libs/javalib libs/ziplib libs/swflib libs/xml-light libs/ttflib}
    @extra_includes = []
    @other_deps = []

    @depends_on_libraries = []
    $deps_hash[@name] = self

    deriving_support if /deriving.*Show|Show.show</.match(open("#{name}.ml").read)

  end

  def depend_on_lib(*args)
    @depends_on_libraries += args
    self
  end

  def link_name
    "#{@name}.CEXT"
  end

  def deriving_support
    return unless DERIVING_SUPPORT
    # for deriving:
    @extra_includes += ["#{DERIVING_PATH}/lib", "#{DERIVING_PATH}/syntax"]
    @extra_deps = "nums.LEXT #{DERIVING_PATH}/lib/show.CEXT"
    @deriving = "-pp #{DERIVING_PATH}/syntax/deriving"
  end

  def cppo
    @cflags_backend = "-pp \"#{CPPO_PATH} #{CFLAGS_BACKEND.join(' ')}\""
    @other_deps << "cppo/cppo" if CPPO_PATH =="cppo/cppo"
  end

  def cmx_dependencies
    cmx_deps_no_ext.map {|v| "#{v}.CEXT"}
  end

  def compile_cmd(native)
    includes = (INCLUDES + (@extra_includes || [])).map {|v| "-I #{v}"}.join(" ")
    "OCAML #{DEBUGGING_SUPPORT} #{ANNOT} #{@cflags_backend} #{@deriving} #{@extra_flags} #{includes} #{@extra_deps} -c #{@name}.ml".ocaml_native(native)
  end

  def tasks(native)
    lib_deps = @depends_on_libraries.map {|v| $libs[v].main_target }
    My_FileTask.new(native, {"#{@name}.CEXT" => (["#{@name}.ml"] + cmx_dependencies + lib_deps + @other_deps)}, [compile_cmd(native)])
  end
end

# implementation start {{{1
# =====================


$repositories = Hash.new
class Repository

  # Repository.new("lib", {:type => :git, :url => ... }
  def initialize(path, repo, depends_on_targets = [])
    @path = path
    @repo = repo
    @extra_targets = []
    repo[:type] ||= :git
    $repositories[path] = self
    @depends_on_targets = depends_on_targets
  end

  # first file: main target
  # additional files: alias targets
  def add_target(files, cmd, depends_on_targets = [])
    @extra_targets << {:files => files, :cmd => cmd, :depends_on_targets => depends_on_targets}
    self
  end

  def main_target
    @extra_targets[][:files][0]
  end

  def clean_command(cmd)
    @clean_command = cmd
    self
  end

  def tasks(native)
    # check out
    cmds = []
    case @repo[:type]
      when :tar_gz
        cmds << "curl '#{@repo[:url]}' | tar xzf -"
        cmds << "mv #{@repo[:rename][0]} #{@repo[:rename][1]}" if @repo[:rename]
      when :git
        cmds << "git clone #{@repo[:url]} #{@path}"
      else raise "not implemented #{@repo[:type]}"
      end
    My_FileTask.new(native, "#{@path}", cmds)

    # compile etc
    @extra_targets.each {|v|
      My_FileTask.new(native, { "#{v[:files][0]}" => ["#{@path}"] + v[:depends_on_targets] }, v[:cmd].ocaml_native(native))

      v[:files].drop(1).each {|file|
        # alias targets
        My_FileTask.new .call(native, { file => v[:files][0] }, [])
      }
    }
    if @clean_command
      My_FileTask.new(native, "clean_#{@path}", @clean_command)
    end
  end
end
Repository.new("libs", {:url => 'git://github.com/HaxeFoundation/ocamllibs.git'})
# good idea to use cppo github url? Or release? Let's hope its stable
Repository.new("cppo", {:url => 'https://github.com/mjambon/cppo.git'}) \
          .add_target(["cppo/cppo"], ["make -C cppo"]) \
          .clean_command(["make -C cppo clean"])

Repository.new("deriving", {:url => "git://github.com/jaked/deriving.git"}, Dir["deriving/**/*.ml", "deriving/**/*.mli"]) \
          .add_target(["deriving/syntax/deriving"], ["make -C deriving"]) \
          .clean_command(["make -C deriving clean"])

Repository.new('extunix', {:url => 'https://forge.ocamlcore.org/frs/download.php/1146/ocaml-extunix-0.0.6.tar.gz', :type => :tar_gz, :rename => ['ocaml-extunix-0.0.6', 'extunix']} )
          .add_target(["extunix/_build/src/extunix.cmxa"], ["cd extunix; ./configure && make"]) \


# getting ml file dependencies from official Makefile - yes this sucks - but is likely to work:

File.open("Makefile","r").each_line {|line|
  case line
  when /([^.]*)\.cmx: (.*)/
    name = $1
    deps = $2.split(" ").map {|v| v.gsub(/.cmx$/, '')}
    $deps_hash[name] = Ml.new(name, deps)
  end
}

$deps_hash["ast"] = Ml.new("ast", []).depend_on_lib(:extlib)
$deps_hash["common"].depend_on_lib(:swflib) if ACTIVE_BACKENDS.include? :swf
$deps_hash["common"].depend_on_lib(:javalib) if ACTIVE_BACKENDS.include? :java
$deps_hash["common"].depend_on_lib(:extc)
$deps_hash["genswf"].depend_on_lib(:ttflib)
$deps_hash["codegen"].depend_on_lib("xml-light".to_sym)

$deps_hash["parser"] = Ml.new("parser", %w{lexer common ast})
$deps_hash["parser"].extra_flags = "-pp camlp4o"
$deps_hash["genneko"].depend_on_lib(:neko)


# which modules use deriving?
# %w{ast}.each {|v| $deps_hash[v].deriving_support }

backend_files = Hash.new
backend_files[:swf] = %w{genswf8 genswf9 genswf}
backend_files[:neko] = %w{genneko}
backend_files[:as3] = %w{genas3}
backend_files[:cpp] = %w{gencpp}
backend_files[:cs] = %w{gencs}
backend_files[:java] = %w{genjava}
backend_files[:js] = %w{genjs}
backend_files[:php] = %w{genphp}

raise "something went wrong" if Set[*ALL_BACKENDS] != Set[*backend_files.keys]
bad_backends = ACTIVE_BACKENDS - ALL_BACKENDS
raise "bad backends #{bad_backends}" if bad_backends.length > 0

$libs = Hash.new

# lib controlled by rake
class Lib

  def initialize(path, name, files)
    @path = path
    @name = name

    files.keys.to_a.each {|k|
      files[k] ||= {}
      v = files[k]
      v[:goal] = case k
      when /(.*)\.c$/; "#{$1}.o"
      when /(.*)\.ml$/; "#{$1}.CEXT"
      else; raise "TODO #{k}"
      end
    }

    @files = files
    @needs = []
    $libs[@path.to_sym] = self
  end

  def main_target; "libs/#{@path}/#{@name}.LEXT"; end

  def add_lib_dependencies(*args); @needs += args; self end

  def clean_task; "clean_#{@path}"; end

  def tasks(native)
    My_CleanTask.new(clean_task, "libs/#{@path}")
    lib_name = "#{@name}.LEXT"
    all_deps = []

    # file tasks
    file_goals = []
    @files.each {|k,v|
      file_deps = v[:deps] || [] 
      goal = "libs/#{@path}/#{v[:goal]}"
      file_goals << goal
      all_deps += [ lib_name, k ]
      file_deps.each {|d| all_deps += [ k, d ] }

      cmds = []
      ocaml_includes = @needs.map {|v| "-I ../#{v}"}
      case k
      when /\.c$/
        cmds << "cd libs/#{@path}; ocamlc #{v[:CFLAGS]} extc_stubs.c".ocaml_native(native)
      when /\.ml$/
        cmds << "cd libs/#{@path}; OCAML #{v[:CFLAGS]} #{ocaml_includes.join(' ')} -c #{k}i".ocaml_native(native) if File.exist? "libs/#{@path}/#{k}i"
        cmds << "cd libs/#{@path}; OCAML #{v[:CFLAGS]} #{ocaml_includes.join(' ')} -c #{k}".ocaml_native(native)
      else; raise "TODO #{k}"
      end
      My_FileTask.new(native,  {goal => ["libs"] + @needs.map {|lib_name| $libs[lib_name].main_target } + (file_deps).map {|d| "libs/#{@path}/#{@files[d][:goal]}"} }, cmds)
    }
    # create library task

    dg = RGL::DirectedAdjacencyGraph.__send__(:[], *all_deps)
    lib_deps = []
    dg.depth_first_visit(lib_name) {|n|
      lib_deps << @files[n][:goal] unless n == lib_name || @files[n][:goal] =~ /\.o$/
    } 

    My_FileTask.new(native, {"#{main_target}" => file_goals },
    ["cd libs/#{@path}; OCAML -a -o #{lib_name} #{lib_deps.join(' ')}".ocaml_native(native)])

    My_Task.new({:compile_libs => [main_target]}, [])
    My_Task.new({@path.to_sym => [main_target]}, [])
  end
end

# lib still controlled by make
class LibMake

  attr_reader :name, :targets, :needs
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

  def tasks(native)
    My_Task.new(clean_task, [ "make -C libs/#{@name} clean"])

    make_args = @make_args.map {|v| " #{v}"}.join('')
    My_FileTask.new(native, {"#{main_target}" => ["libs"] + prerequisites}, ["make -C libs/#{@name} #{native == "native" ? "native" : "bytecode"}"])

    # for each alternative target create a new task:
    # targets.drop(1).each do |t|
    #   my_multifile "libs/#{@name}/#{t}" => lib.main_target
    # end

    My_Task.new({:compile_libs => [main_target]}, [])
    My_Task.new({name.to_sym => [main_target]}, [])
  end

  def clean_task; "clean_#{@name}"; end
end
deps_enum = {:deps => %w{enum.ml}}
Lib.new("extlib", "extLib", {
    "enum.ml" => nil,
    "bitSet.ml" => {:deps => %w{enum.ml}},
    "dynArray.ml" => {:deps => %w{enum.ml}},
    "extArray.ml" => {:deps => %w{enum.ml bitSet.ml}},
    "extHashtbl.ml" => {:deps => %w{enum.ml}},
    "extList.ml" => {:deps => %w{enum.ml}},
    "extString.ml" => {:deps => %w{enum.ml}},
    "global.ml" => nil,
    "IO.ml" => {:deps => %w{extString.ml}},
    "option.ml" => nil,
    "pMap.ml" => {:deps => %w{enum.ml}},
    "std.ml" => {:deps => %w{enum.ml}},
    "uChar.ml" => nil,
    "uTF8.ml" => {:deps => %w{uChar.ml}},
    "base64.ml" => {:deps => %w{IO.ml}},
    "unzip.ml" => {:deps => %w{IO.ml}},
    "refList.ml" => {:deps => %w{extList.ml}},
    "optParse.ml" => {:deps => %w{extString.ml extList.ml}},
    "dllist.ml" => {:deps => %w{enum.ml}},
    "multiArray.ml" => nil })

Lib.new("extc", "extc", {
          "extc.ml" => nil,
          "extc_stubs.c" => {:CFLAGS => "-I zlib"}
        }) \
    .add_lib_dependencies(:extlib)
Lib.new("neko", "neko", {
          "nast.ml" => nil,
          "nxml.ml" => {:deps => %w{nast.ml}},
          "binast.ml" => {:deps => %w{nast.ml}},
          "nbytecode.ml" => nil,
          "ncompile.ml" => {:deps => %w{nbytecode.ml}},
        }).add_lib_dependencies(:extlib)

LibMake.new(:javalib, ["java.LEXT"]).add_lib_dependencies(:extlib)
LibMake.new(:ziplib, ["zip.LEXT"]).add_lib_dependencies(:extc)
LibMake.new(:swflib, ["swflib.LEXT"]).add_lib_dependencies(:extlib, :extc)
LibMake.new("xml-light".to_sym, ["xml-light.LEXT"]).add_make_args("xml-light.LEXT")
# circular dependencies, there is not much you could optimize ..
# Lib.new("xml-light", "xml-light", {
#           "xml_lexer.ml" => {:deps => %w{xmlParser.ml}},
#           "dtd.ml" => {:deps => %w{xml_lexer.ml}},
#           "xmlParser.ml" => {:deps => %w{dtd.ml}},
#           "xml.ml" => {:deps => %w{}},
#         })
LibMake.new(:ttflib, ["ttf.LEXT"]).add_lib_dependencies(:swflib, :extlib)

# drop unused files:
backend_files.each_pair do |k,v|
  if (ACTIVE_BACKENDS.include? k)
    CFLAGS_BACKEND << "-D BACKEND_#{k}"
  else
    v.each do |file_to_delete|
      $deps_hash.delete file_to_delete
      $deps_hash.each_pair {|k,ml| ml.cmx_deps_no_ext = ml.cmx_deps_no_ext.select {|file| file != file_to_delete }}
    end
  end
end

# which modules require cppo?
%w{common interp main gencommon typer}.each {|v| $deps_hash[v].cppo }

deps = []
$deps_hash.each_pair {|k,ml| ml.cmx_deps_no_ext.each {|dep| deps << k; deps << dep } }
dg = RGL::DirectedAdjacencyGraph.__send__(:[], *deps )

# rake tasks {{{1
# ===============

HELP_TEXT=<<-EOF
usage:

drake haxe.native
drake haxe.bytecode

drake haxe (will build both)

useful targets for libs:
========================
drake clean_LIB (LIB like swflib)
drake libs/swflib/swflib.cmxa

drake clean_libs

checking out
=========================
drake libs # gets libs
drake cppo # gets cppo/
drake derving # gets deriving


# build deriving:
drake deriving/syntax/deriving

# build cppo:
drake cppo/cppo



info
============================
libs/*: some Makefiles are no longer used. See LibMake usage in Rakefile


state
============================
If you change backends you have to drake haxe_clean, then recompile

EOF

task :help do
  puts HELP_TEXT
end

# cleaning {{{2
def delete_files(*files)
  files.each {|v|
    File.delete v if File.exist? v
  }
end

def clean_dir(path)
  files = Dir.__send__(:[], *EXTS_TO_CLEAN.map {|v| "#{path}/#{v}" })
  puts "cleaning #{files}"
  delete_files(*files)
end

task :clean do
  # should this clean everything?
  raise "there is no :clean target. Try clean_haxe, clean_libs, clean_all"
end

My_CleanTask.new(:clean_haxe, "./", ["haxe.native", "haxe.bytecode"])

task :clean_graph do
  delete_files("graph.jpg","graph.dot")
end

My_Task.new({:clean_all => [:clean_haxe, :clean_libs, :clean_graph]}, [])

# dependency graph of ./*.ml files: {{{2

file "graph.jpg" do
  # Use DOT to visualize this graph:
  require 'rgl/dot'
  dg.write_to_graphic_file('jpg')
end

# haxe and its files {{{2

# rake targets {{{3


My_Task.new({:default => [:help] }, [])


# cleaning:
My_Task.new({:clean_libs => $libs.map {|k,v| v.clean_task}}, [])

# building
ACTIVE_FLAVOURS.each do |flavour|
  native = flavour.clone
  $repositories.each_pair {|k,r| r.tasks(flavour) }
  $deps_hash.each_pair {|k,ml| ml.tasks(flavour) }
  $libs.each_pair {|k,v| v.tasks(flavour) }

  # haxe target
  haxe_local_deps = []
  require "rgl/traversal"
  dg.depth_first_visit("main") {|n|
    haxe_local_deps << "#{$deps_hash[n].link_name}"
  }
  haxe_local_deps

  libs_deps = [:extc, :ziplib]
  libs_deps << :ttflib if ACTIVE_BACKENDS.include? :swf

  libs_deps.map! {|v| $libs[v].main_target }
  haxe_goal = "haxe.#{native}"

  # TODO
  libs = []
  libs << "-cclib"
  libs << "libs/extc/extc_stubs.o"
  libs << "-cclib"
  libs << "-lz"
  libs << "unix.LEXT"
  libs << "str.LEXT"
  libs << "libs/extlib/extLib.LEXT"
  libs << "libs/xml-light/xml-light.LEXT"
  libs << "libs/swflib/swflib.LEXT" if ACTIVE_BACKENDS.include? :swf
  libs << "libs/extc/extc.LEXT"
  libs << "libs/neko/neko.LEXT"
  libs << "libs/javalib/java.LEXT" if ACTIVE_BACKENDS.include? :java
  libs << "libs/ziplib/zip.LEXT" if ACTIVE_BACKENDS.include? :java or ACTIVE_BACKENDS.include? :swf
  libs << "libs/ttflib/ttf.LEXT" if ACTIVE_BACKENDS.include? :swf
  if DERIVING_SUPPORT
    libs << ["-I #{DERIVING_PATH}/lib", " -I #{DERIVING_PATH}/syntax"]
    libs << ["nums.LEXT", "show.CEXT"]
  end

  My_FileTask.new(native, {haxe_goal => ["libs/"] + haxe_local_deps + libs_deps},
    [ "OCAML #{DEBUGGING_SUPPORT} #{native == "native" ? "" : "-custom"} #{libs.join(' ')} -o #{haxe_goal} #{haxe_local_deps.join(' ')}".ocaml_native(native) ]
  )
end

My_Task.new({ :haxe => ACTIVE_FLAVOURS.map {|v| "haxe.#{v}"} }, [])

$tasks.each {|v| v.rake_task }

class MakefileWriter
  def initialize()
    @items = []
    @cache = Hash.new
  end

  def write(s)
    @items << s unless @cache.include? s
    @cache[s] = true
  end

  def string
    return @items.join("\n")
  end
end

task :makefile do
  out = MakefileWriter.new

  out.write(<<-EOF)
# this makefile was created by Rakefile
# TODO: don't depend on libraries if backends are dropped
# you always have to enable BACKEND_neko
#
# important targets:
# haxe: creates haxe.native and haxe.bytecode
# haxe_clean: cleans haxe (has to be run if you enable disable backends,
# because timestamps don't change..
  EOF

  out.write("# BACKEND_X=yes: comment to disable backend")
  backend_files.each_pair {|k,v|
    out.write("#{k == :neko ? "" : "# "}BACKEND_#{k}=yes")
  }
  out.write("")

  out.write("")
  HELP_TEXT.split("\n").each {|v|
    out.write("# #{v.gsub("drake", "make")}")
  }
  out.write("")


  $tasks.each {|v| v.makefile(out) }

  makefile = out.string

  backend_files.each_pair {|k,v|
    makefile = makefile.gsub("-D BACKEND_#{k}", "$(if ${BACKEND_#{k}}, -D BACKEND_#{k},)")

    v.each {|file|
      # mind the space, this does not drop rules at ^
      makefile = makefile.gsub(" #{file}.cmx", " $(if ${BACKEND_#{k}}, #{file}.cmx,)")
      makefile = makefile.gsub(" #{file}.cmo", " $(if ${BACKEND_#{k}}, #{file}.cmo,)")
    }
  }

  File.open('makefile-generated-by-rake', "wb") { |file| file.write(makefile) }
end
