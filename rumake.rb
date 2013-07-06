# encoding: UTF-8

require_relative "lib-ruby/rumake/task.rb"
include Rumake
# no longer using rake nor drake. Both didn't fit my needs.
# Be simple but accurate



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


require "set"
require 'rgl/adjacency'
require "rgl/traversal"
require 'rgl/connected_components'
require 'rgl/topsort'

ALL_BACKENDS= [:neko, :as3, :cpp, :cs, :js, :php, :java, :swf]
ACTIVE_BACKENDS = [:neko]


BACKEND_FILES = Hash.new
BACKEND_FILES[:swf] = %w{genswf8.ml genswf9.ml genswf.ml}
BACKEND_FILES[:neko] = %w{genneko.ml}
BACKEND_FILES[:as3] = %w{genas3.ml}
BACKEND_FILES[:cpp] = %w{gencpp.ml}
BACKEND_FILES[:cs] = %w{gencs.ml}
BACKEND_FILES[:java] = %w{genjava.ml}
BACKEND_FILES[:js] = %w{genjs.ml}
BACKEND_FILES[:php] = %w{genphp.ml}

# utils {{{1
# ==========


$once_set = Set.new
def once(name)
  if not $once_set.include? name
    yield
    $once_set << name
  end
end


INCLUDES=%w{}
CFLAGS_BACKEND = []
EXTS_TO_CLEAN = ["*.cmx", "*.cmi", "*.cmo", "*.cma", "*.cmxa", "*.annot", "*.o","*.a", "*.cmt", "*.cmti"]

def clean_dir(path)
  files = Dir.__send__(:[], *EXTS_TO_CLEAN.map {|v| "#{path}/#{v}" })
  puts "cleaning #{files}"
  delete_files(*files)
end

def delete_files(*files)
  files.each {|v|
    File.delete v if File.exist? v
  }
end

class ArrayOfHashes
  def initialize(*hashes)
    @hashes = hashes
  end

  def values(key); @hashes.map {|v| v[key]}.compact; end

  def first(key) values(key).first; end
  def join_array(key) values(key).flatten(1); end
end


# assertions {{{2

class Object
  def assert_not_null; self; end
  def assert_array; assert "should be a list"; end
end

class Null
  def assert_not_null; raise "should not have been null"; end
end

class Arary
  def assert_array; self; end
end


# ocaml(c/opt) {{{2



class OcamlLinkFlags
  def initialize(*args)
    @args = args
  end
  def ocaml_link_flags; @args; end
  def ocaml_cflags; []; end
end

class LibMake

  attr_reader :o
  def initialize(opts)
    @o = opts
    cc = opts[:compiler]
    @o[:depends_on] = []
    @o[:target_rel] = @o[:target_rel].gsub(/\LIB_EXT$/, cc.lib_ext)
    @o[:target_absolute] = File.join(@o[:base_dir], @o[:target_rel])
    @name = File.basename(@o[:base_dir])
  end

  def ocaml_cflags; ["-I", File.absolute_path(@o[:base_dir])]; end
  def ocaml_link_flags; o.fetch(:lib_flags, []) + [main_target]; end 

  def main_target; @o[:target_absolute]; end

  def tasks
    once clean_task do
      My_Task.new({clean_task => []}, [ "make -C libs/#{@name} clean"])
    end

    make_args = (@o.fetch(:make_args_by_compiler_type, {}).fetch(@o[:compiler].type, []))
    My_FileTask.new({main_target => ["libs"] + @o.fetch(:depends_on, [])},
    [ "make -C libs/#{@name} #{@o[:compiler].type == "native" ? "native" : "bytecode"}",
      # we track more dependencies than the makefile, so the makefile may not
      # touch the library file causing rebuilds again and again, so update the
      # timestamp
      "touch #{main_target}"
    ]
    )

    # TODO refactor?, make it an configuration option
    once @name.to_sym do
      My_Task.new({@name.to_sym => [main_target]}, [])
    end

    # should be using make clean? TODO

    $libs_to_clean_tasks << clean_task
  end

  def clean_task; "clean_#{@name}"; end
end

class OcamlCC
  def compile_file(*options)
    o = ArrayOfHashes.new(*options)
    base_dir = o.first(:base_dir)
    action = o.first(:action)
    out = o.first(:out)

    libs_cflags = o.join_array(:depends_on).flatten(1).map{|v|
      v.respond_to?(:ocaml_cflags) ?  v.ocaml_cflags : []
    }.join(' ')
    libs_cflags += o.join_array(:cflags).join(' ')

    cppo_executable_fun = o.first(:cppo_executable_fun)

    # cppo
    cppo_flags = o.join_array(:cppo_flags)
    use_cppo = (o.values(:use_cppo).detect {|v| !!v}) || (cppo_flags.length > 0)

    # camlp4
    camlp4_flags = o.join_array(:camlp4_flags)
    camlp4_suggestions = o.values(:camlp4) || ['camlp4']
    raise "multiple camlp4 suggestions #{camlp4_suggestions}" if camlp4_suggestions.length > 1
    camlp4 = camlp4_suggestions.first

    includes     = o.join_array(:includes)
    dependencies = o.join_array(:dependencies).flatten(1)

    pre_flags = ""
    pre_flags = ""

    cmd = case action
    when :compile_ml
      in_ = o.first(:in)
      pre = ""

      if action != :link && use_cppo then
        # this is hacky: run cppo, replacing original file
        pre = "
        grep -q '#ifdef' #{in_} && {
        cp #{in_}  #{in_}.bak;
        #{cppo_executable_fun.call} #{cppo_flags.join(' ')} #{o.first(:in)} > #{in_}.cppo;
        mv #{in_}.cppo #{in_};
        sed -i 's/^#.*//' #{in_}
        } || true;"
      end

      [ 
      pre,
      cc,
      libs_cflags,
      includes.map {|v| "-I #{v}"},
      action != :link && camlp4 ? "-pp '#{camlp4} #{camlp4_flags.join(' ')}'" : "",
      # second -pp arg is executed first !
      # action != :link && use_cppo ? "-pp '#{cppo_executable_fun.call} #{cppo_flags.join(' ')}'" : "",
      dependencies,
      "-o #{o.first(:out)} -c #{in_}"
      ]
    when :link_lib
      [ cc,
      "-a -o #{o.first(:out)}",
      dependencies
      ]
    when :link_executable
      [ cc,
      "#{custom} -g -o #{o.first(:out)}",
      dependencies
      ]
    else raise "unexpected #{action}"
    end
    cmd.join(' ')
  end

  def cc_compile(*args)
    compile_file({:action => :compile_ml}, *args)
  end
  def cc_link(*args)
    compile_file(*args)
  end

  def ml_mod_ext(s); s.gsub(/ml$/, ext_mod).gsub(/mli$/, "cmi"); end
end

class Ocamlopt < OcamlCC
  def exe_suffix; ""; end
  def ext_mod; "cmx"; end
  def lib_ext; "cmxa"; end
  def cc; "ocamlopt.opt"; end
  def type; "native"; end
  def custom; ""; end
end

class OcamlC < OcamlCC
  def exe_suffix; ".byte"; end
  def ext_mod; "cmo"; end
  def lib_ext; "cma"; end
  def cc; "ocamlc.opt"; end
  def type; "bytecode"; end
  def custom; "-custom"; end
end

class Array
  # bad style
  def ocaml_as_includes; self; end
  def ocaml_link_flags; self; end
end

class Symbol
  def main_target; self; end
end

class String
  def main_target; self; end
end

class Hash
  # bad style
  def rewrite_deps
    Hash[self.map {|k, list| [k, list.select {|v| v.respond_to? :main_target}.map {|v| v.main_target} ] } ]
  end
end

# dgl {{{2

class Hash
  def to_adjecent
    r = []
    each_pair {|k,list| (list || []).each {|v| r << k; r << v } }
    r
  end
end

class Array
  def count_unique_items
    h = Hash.new
    each {|v| h[v] ||= 0; h[v] += 1}
    h
  end
end


# task abstraction {{{2

$tasks = []

# provides makefile implementation for My_FileTask My_Task
class My_MakeFileTask
  def makefile(out)
    @hash.each_pair {|k,v|
      out.write(<<-EOF)
#{k}: #{v.join(' ')}
#{@cmds.map {|v| "\t#{v}"}.join("\n")}
      EOF
    }
  end
end


# A directory task, like My_FileTask, but no timestamp checking
class My_DirTask < My_MakeFileTask
  include Rumake

  # hash is target => deps
  def initialize(hash, cmds)
    @hash = hash.rewrite_deps
    raise "unexpected" if @hash.keys.length > 1
    @cmds = cmds
    $tasks << self
  end

  def rumake_task
    dir @hash do
      @cmds.each {|v| sh v}
    end
  end

end

# a File task. target is a file or directory
class My_FileTask < My_MakeFileTask
  include Rumake

  # hash is target => deps
  def initialize(hash, cmds)
    hash.values.flatten(1).map {|v| v.assert_not_null}
    @hash = hash.rewrite_deps
    raise "unexpected" if @hash.keys.length > 1
    @cmds = cmds
    $tasks << self
  end

  def rumake_task
    file @hash.rewrite_deps do
      @cmds.each {|v|
        sh v
        puts "done with #{v}"
      }
    end
  rescue Exception => e
    puts "for target #{@hash.keys}"
    raise e
  end

end

# a "phony" task, target file/dir will not exist (used for aliasing task names)
class My_Task < My_MakeFileTask
  include Rumake

  def initialize(hash, cmds)
    @hash = hash
    @cmds = cmds
    $tasks << self
  end

  def rumake_task
    task @hash do
      @cmds.each {|v| sh v}
    end
  end

end

class My_CleanTask
  include Rumake

  def initialize(name, dir, files = [])
    @name = name
    @files = files
    @dirs = dir
    @dirs = [@dirs] unless @dirs.is_a? Array
    $tasks << self
  end

  def rumake_task
    task @name do
      @dirs.each {|dir| clean_dir dir }
      # TODO clean files
    end
  end

  def makefile(out)
    out.write("#{@name}:\n")
    @dirs.each {|dir|
      out.write("\tcd #{@dir}; rm #{EXTS_TO_CLEAN.join(' ')} #{@files.join(' ')}\n")
    }
  end
end

# this library {{{2

# ocaml buildable targets:
# executable
# library
class OcamlBuildable

  attr_reader :o
  def initialize(o)
    @o = o
    cc = @o[:compiler]

    @o[:base_dir].assert_not_null
    # executable name or library without extension
    if @o[:type] == :ocaml_library
      @o[:target_rel] = @o[:target_rel].gsub(/\LIB_EXT$/, cc.lib_ext)
    end
    @o[:target_absolute] = File.join(@o[:base_dir], @o[:target_rel])

    # either paths, or things responding to ocaml_link_flags(compiler), ocaml_cflags
    @o[:depends_on] ||= []

    @o[:files].keys.to_a.each {|file|
      file_o = @o[:files][file]
      file_o[:camlp4] = 'camlp4o'
      if @o.fetch(:mlis, false) || file_o.fetch(:mli, false)
        @o[:files]["#{file}i"] = file_o.clone
        file_o[:ml_deps] = ["#{file}i"]
      end
    }

    # name => { options }
    # keys for options:
    #  :ml_deps: name of .ml files (same :base_dir)
    #  :depends_on (see above)
    @o[:files].each_pair {|file, file_o|
      file_o[:ml_deps] ||= {}
    }

    # optional, something like {"foo.ml" :=>  %w{bar.ml moon.ml}}
    @o[:ml_deps] ||= Hash.new

    @o[:patches] ||= []

    @o[:annot] ||= true      # -dtypes option
    @o[:bin_annot] ||= false # -bin-annot option
    @o[:debug] ||= true # -g option
  end

  def prepare
    begin
      o =  Marshal.load( Marshal.dump(@o) )
      compiler = o[:compiler]
      @o[:patches].each {|v|
        v.patch_ocaml_buildable_options o
      }

      # set :absolute_path for each file
      o[:files].each_pair {|k,v| o[:files][k][:absolute_path] = File.join(o[:base_dir], k) }

      # sort files by dependency order
      x = Hash[o[:files].map {|k, v| [k, v[:ml_deps]]}].to_adjecent
      dg = RGL::DirectedAdjacencyGraph[*x]

      # cycles? not supported yet. Must be all passed to one ocaml(c/opt)
      # command?
      comp_map = dg.strongly_connected_components.comp_map
      raise "cycles detected ! #{comp_map} " if comp_map.values.count_unique_items.select {|k,v| v > 1}.size > 0

      # now use topsort without missing items ..
      sorted = dg.topsort_iterator.to_a

      # files could have been dropped by patchers, only the :files array is valid
      files = o[:files].keys
      files_without_dependency_info = (files - sorted)
      o[:files_sorted] = sorted.reverse + (files - sorted)
      o
    rescue Exception => e
      puts "while preparing #{@o[:target]}"
      raise e
    end
  end

  def tasks
    o = prepare
    cc = o[:compiler]

    annot_flags = (o[:annot] ? " -annot" : "") \
        + (o[:bin_annot] ? " -bin-annot" : "") \
        + (o[:debug] ? " -g" : "")
    annot = {:cflags => annot_flags}

    target_depends_on = []
    mls_for_target = []
    # ml file tasks, .o tasks
    o[:files].each_pair {|file, file_o|
      depends_on = file_o.fetch(:depends_on, [])
      file_o[:out_rel] = cc.ml_mod_ext(file).gsub(/\.c$/,'.o')
      file_o[:out_absolute] = File.join(o[:base_dir], file_o[:out_rel])

      target_depends_on += depends_on

      depends_on += o[:depends_on]

      cppo_dep = []

      cppo_executable_fun = lambda {
        cppo_dep = ["cppo/cppo"]
        File.absolute_path("cppo/cppo")
      }
      case file
      when /\.ml$/
        ml_deps = (o[:files_sorted] && file_o[:ml_deps]).map {|v| File.join(o[:base_dir], cc.ml_mod_ext(v) ) }
        cmd = "cd #{o[:base_dir]}; #{cc.cc_compile({:cppo_executable_fun => cppo_executable_fun,:out => file_o[:out_rel], :depends_on => depends_on, :in => file}, annot, o, file_o)}"
        My_FileTask.new({file_o[:out_absolute] => cppo_dep + [file_o[:absolute_path]] + ml_deps + depends_on + o[:depends_on]}, [cmd])
        mls_for_target << file
      when /\.mli$/
        once file_o[:out_absolute] do
          ml_deps = (o[:files_sorted] && file_o[:ml_deps]).map {|v| File.join(o[:base_dir], cc.ml_mod_ext(v) ) }
          cmd = "cd #{o[:base_dir]}; #{cc.cc_compile({:cppo_executable_fun => cppo_executable_fun, :out => file_o[:out_rel], :depends_on => depends_on, :in => file}, o, file_o)}"

          My_FileTask.new({file_o[:out_absolute] => cppo_dep + [File.join("./", file_o[:absolute_path])] + ml_deps + depends_on + o[:depends_on]}, [cmd])
        end
      when /\.c$/
        once file_o[:out_absolute] do
          cmd = "cd #{o[:base_dir]}; ocamlc #{file_o.fetch(:CFLAGS, []).join(' ')} #{file}"
          My_FileTask.new({file_o[:out_absolute].gsub(/\.c$/, '.o') => [file_o[:absolute_path]] + o[:depends_on]}, [cmd])
        end
      else
        raise "unexpected #{file.inspect}"
      end


      # case file
      # when /\.ml$/
      #   ml_deps = (o[:files_sorted] && file_o[:ml_deps]).map {|v| File.join(o[:base_dir], cc.ml_mod_ext(v) ) }
      #   cmd = "#{cc.cc_compile({
      #     :includes => o[:base_dir],
      #     :cppo_executable_fun => cppo_executable_fun,
      #     :out => "#{o[:base_dir]}/#{file_o[:out_rel]}", 
      #     :depends_on => depends_on, 
      #     :in => "#{o[:base_dir]}/#{file}"
      #   }, annot, o, file_o)}"
      #   My_FileTask.new({file_o[:out_absolute] => cppo_dep + [file_o[:absolute_path]] + ml_deps + depends_on + o[:depends_on]}, [cmd])
      #   mls_for_target << file
      # when /\.mli$/
      #   ml_deps = (o[:files_sorted] && file_o[:ml_deps]).map {|v| File.join(o[:base_dir], cc.ml_mod_ext(v) ) }
      #   cmd = "#{cc.cc_compile({:cppo_executable_fun => cppo_executable_fun,
      #     :includes => o[:base_dir],
      #     :out => "#{o[:base_dir]}/#{file_o[:out_rel]}",
      #     :depends_on => depends_on,
      #     :in => "#{o[:base_dir]}/#{file}"
      #   },
      #   o, file_o)}"
      #   My_FileTask.new({file_o[:out_absolute] => cppo_dep + [File.join("./", file_o[:absolute_path])] + ml_deps + depends_on + o[:depends_on]}, [cmd])
      # when /\.c$/
      #   cmd = "cd #{o[:base_dir]}; ocamlc #{file_o.fetch(:CFLAGS, []).join(' ')} #{file}"
      #   My_FileTask.new({file_o[:out_absolute].gsub(/\.c$/, '.o') => [file_o[:absolute_path]] + o[:depends_on]}, [cmd])
      # else
      #   raise "unexpected #{file.inspect}"
      # end
    }

    target_depends_on += o[:depends_on]

    # main executable or library task?
    out_rel = o[:target_rel]
    out_absolute = File.join( o[:base_dir], out_rel)

    case o[:type]
    when :ocaml_executable; 
      opts = {
        :action => :link_executable,
        :out => out_rel,
        :dependencies => (target_depends_on.uniq.map {|v|
          v.respond_to?(:ocaml_link_flags) ?  v.ocaml_link_flags : []
        }.flatten(1)) \
                         +  (o[:files_sorted].select {|v| not v =~ /\.(c|mli)$/}).map {|v| cc.ml_mod_ext(v) }
      }
    when :ocaml_library; 
      opts = {
        :action => :link_lib,
        :out => out_rel,
        :dependencies => (o[:files_sorted].select {|v| not v =~ /\.(c|mli)$/}).map {|v| cc.ml_mod_ext(v) }
      }
    end
    cmd = "cd #{o[:base_dir]}; #{cc.cc_link(opts, o)}"
    My_FileTask.new({out_absolute => target_depends_on + o[:files].map {|k,v| v[:out_absolute]}}, [cmd])

    if not @o[:alias_for_main_task].nil?
      My_Task.new({@o[:alias_for_main_task] => [out_absolute]}, [])
      if not $tasks.include? "clean_#{@o[:alias_for_main_task]}"
        My_CleanTask.new("clean_#{@o[:alias_for_main_task]}", o[:base_dir])
        $libs_to_clean << o[:base_dir]
      end
    end
    self
  end

  def main_target
    @o[:target_absolute]
  end
end

class OcamlLib < OcamlBuildable
  def initialize(opts)
    opts[:type] = :ocaml_library
    super(opts)
  end
  def to_s
    "#<#{self.class.name}:#{object_id} #{@o[:target_rel]}>"
  end
  def ocaml_cflags
    ["-I", File.absolute_path(@o[:base_dir])]
  end
  def ocaml_link_flags
    ocaml_cflags + [File.basename(main_target)] + o.fetch(:propagate_link_flags, [])
  end
end

class OcamlExecutable < OcamlBuildable
  def initialize(opts)
    opts[:type] = :ocaml_executable
    super(opts)
  end
end

# this project {{{2

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

  def tasks
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
    My_DirTask.new({"#{@path}" => []}, cmds)

    # compile etc
    @extra_targets.each {|v|
      My_FileTask.new({ "#{v[:files][0]}" => ["./#{@path}"] + v[:depends_on_targets] }, v[:cmd])

      v[:files].drop(1).each {|file|
        # alias targets
        My_FileTask.new .call({ file => v[:files][0] }, [])
      }
    }
    if @clean_command
      My_FileTask.new({"clean_#{@path}" => []}, @clean_command)
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
          .add_target(["extunix/_build/src/extunix.LEXT"], ["cd extunix; ./configure && make"]) \


class HaxeBackends
  def initialize(active_backends)
    @all_backends = ALL_BACKENDS
    @active_backends = active_backends
  end

  def patch_ocaml_buildable_options(o)
    files = o[:files]
    files_to_delete = []
    @all_backends.each {|backend|
      if not @active_backends.include? backend
        BACKEND_FILES[backend].each {|file| 
          files_to_delete << file
          files.delete file
        }
      end
    }
    files.each_pair {|k,v|
      v[:ml_deps] ||= []
      v[:ml_deps].select! {|v| not files_to_delete.include? v}
    }
    cppo_flags = @active_backends.map {|v| "-D BACKEND_#{v}"}
    %w{common.ml interp.ml main.ml gencommon.ml typer.ml}.each {|v| 
      files[v][:cppo_flags] = cppo_flags
    }
  end
end


# haxe executable {{{3
ENABLE_TRACING = true


class Tracing
  def patch_ocaml_buildable_options(o)
    files = o[:files]
    files.each_pair {|k,v|
      v[:camlp4_flags] ||= []
      v[:camlp4_flags] << File.absolute_path("trace/instr.cmo")
      v[:depends_on] ||= []
      v[:depends_on] << "trace/instr.cmo"
    }
  end
end
$tracing = [Tracing.new]

$libs_to_clean_tasks = Set.new
$libs_to_clean = Set.new

%w{trace Camlp4Tracer instr}.each {|v|
My_FileTask.new({"trace/#{v}.cmo" => ["trace/#{v}.ml"]},
  [ "cd trace; ocamlc -dtypes -pp 'camlp4r -I +camlp4 -parser Camlp4QuotationCommon  -parser Camlp4QuotationExpander' -I +camlp4 -o #{v}.cma -c #{v}.ml" ])
}
$repositories.each_pair {|k,r| r.tasks }

[Ocamlopt.new, OcamlC.new] .each do |compiler|

  OcamlExecutable.new({
    :alias_for_main_task => "tracetest#{compiler.exe_suffix}",
    :patches => $tracing,
    :base_dir => "./",
    :target_rel => "tracetest#{compiler.exe_suffix}",
    :files => {"tracetest.ml" => {}},
    :compiler => compiler,
  }).tasks

libs = Hash.new
$libs = libs
def l(*args); $libs.values_at(*args); end

lib_tracing = $tracing

# TODO dependencies
libs[:extlib] = OcamlLib.new({
  :base_dir => 'libs/extlib',
  :target_rel => 'extLib.LIB_EXT',
  :compiler => compiler,
  :alias_for_main_task => "extlib.#{compiler.type}",
  :patches => lib_tracing,
  :mlis => true,
  :propagate_link_flags => ["-cclib", "libs/extc/extc_stubs.o", '-cclib', '-lz' ],
  :files => {
    "enum.ml" => {},
    "bitSet.ml" => {:ml_deps => %w{enum.ml}},
    "dynArray.ml" => {:ml_deps => %w{enum.ml}},
    "extArray.ml" => {:ml_deps => %w{enum.ml bitSet.ml}},
    "extHashtbl.ml" => {:ml_deps => %w{enum.ml}},
    "extList.ml" => {:ml_deps => %w{enum.ml}},
    "extString.ml" => {:ml_deps => %w{enum.ml}},
    "global.ml" => {},
    "IO.ml" => {:ml_deps => %w{extString.ml}},
    "option.ml" => {},
    "pMap.ml" => {:ml_deps => %w{enum.ml}},
    "std.ml" => {:ml_deps => %w{enum.ml}},
    "uChar.ml" => {},
    "uTF8.ml" => {:ml_deps => %w{uChar.ml}},
    "base64.ml" => {:ml_deps => %w{IO.ml}},
    "unzip.ml" => {:ml_deps => %w{IO.ml}},
    "refList.ml" => {:ml_deps => %w{extList.ml}},
    "optParse.ml" => {:ml_deps => %w{extString.ml extList.ml option.ml refList.ml}},
    "dllist.ml" => {:ml_deps => %w{enum.ml}},
    "multiArray.ml" => {}
  }
})

libs[:extc] = OcamlLib.new({
  :base_dir => 'libs/extc',
  :compiler => compiler,
  :target_rel => 'extc.LIB_EXT',
  :alias_for_main_task => "extc.#{compiler.type}",
  :patches => lib_tracing,
  :files => {
    "extc.ml" => {:depends_on => l(:extlib)},
    "extc_stubs.c" => {:CFLAGS => %w{-I zlib}}
  }
})
libs[:neko] = OcamlLib.new({
  :base_dir => "libs/neko",
  :compiler => compiler,
  :target_rel => "neko.LIB_EXT",
  :alias_for_main_task => "neko.#{compiler.type}",
  :patches => lib_tracing,
  :files => {
    "nast.ml" => {},
    "nxml.ml" => {:ml_deps => %w{nast.ml}},
    "binast.ml" => {:ml_deps => %w{nast.ml}},
    "nbytecode.ml" => {},
    "ncompile.ml" => {:ml_deps => %w{nbytecode.ml}},
  }
})
libs[:javalib] = LibMake.new({
  :base_dir => "libs/javalib",
  :compiler => compiler,
  :target_rel => "java.LIB_EXT",
  :alias_for_main_task => "javalib.#{compiler.type}",
})
libs[:ziplib] = LibMake.new({
  :base_dir => "libs/ziplib",
  :compiler => compiler,
  :target_rel => "zip.LIB_EXT",
  :alias_for_main_task => "ziplib.#{compiler.type}",
})
libs[:swflib] = LibMake.new({
  :base_dir => "libs/swflib",
  :compiler => compiler,
  :target_rel =>  "swflib.LIB_EXT",
  :alias_for_main_task => "swflib.#{compiler.type}",
})

libs[:xml_light] = LibMake.new({
    :base_dir => "libs/xml-light", 
    :compiler => compiler,
    :target_rel => "xml-light.LIB_EXT", 
    :alias_for_main_task => "xml-light.#{compiler.type}",
    :make_args_by_compiler_type => {"native" => "xml-light.cmxa", "bytecode" => "xml-light.cma"}
})
# circular dependencies, there is not much you could optimize ..
# Lib.new("xml-light", "xml-light", {
#           "xml_lexer.ml" => {:deps => %w{xmlParser.ml}},
#           "dtd.ml" => {:deps => %w{xml_lexer.ml}},
#           "xmlParser.ml" => {:deps => %w{dtd.ml}},
#           "xml.ml" => {:deps => %w{}},
#         })
libs[:ttflib] = LibMake.new({
  :base_dir => "libs/ttflib", 
  :compiler => compiler,
  :alias_for_main_task => "ttflib.#{compiler.type}",
  :target_rel => "ttf.LIB_EXT"
})

libs[:extc].o[:depends_on] << libs[:extlib]
libs[:neko].o[:depends_on] << libs[:extlib]
libs[:javalib].o[:depends_on] << libs[:extlib]
libs[:ziplib].o[:depends_on] << libs[:extc]
libs[:swflib].o[:depends_on] += [libs[:extlib], libs[:extc]]
libs[:ttflib].o[:depends_on] += [libs[:swflib], libs[:extlib]]

  # getting ml file dependencies from official Makefile - yes this sucks - but is likely to work:

  files = {
    "ast.ml" => {:depends_on => l(:extlib)},
    "codegen.ml"=>{:ml_deps=>["optimizer.ml", "typeload.ml", "typecore.ml", "type.ml", "genxml.ml", "common.ml", "ast.ml"], :depends_on => l(:xml_light, :extlib)},
    "common.ml"=>{:ml_deps=>["type.ml", "ast.ml"], :depends_on => l(:extc, :extlib, :xml_light)},
    "dce.ml"=>{:ml_deps=>["ast.ml", "common.ml", "type.ml"], :depends_on => l(:extlib)},
    "genas3.ml"=>{:ml_deps=>["type.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "gencommon.ml"=>{:ml_deps=>["type.ml", "common.ml", "codegen.ml", "ast.ml"], :depends_on => l(:extlib)},
    "gencpp.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "gencs.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "gencommon.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "genjava.ml"=>{:ml_deps=>["type.ml", "gencommon.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "genjs.ml"=>{:ml_deps=>["type.ml", "optimizer.ml", "lexer.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "genneko.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "common.ml", "codegen.ml", "ast.ml"], :depends_on => l(:neko, :extlib)},
    "genphp.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "genswf.ml"=>{:ml_deps=>["type.ml", "genswf9.ml", "genswf8.ml", "common.ml", "ast.ml"]},
    "genswf8.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "genswf9.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "genswf8.ml", "common.ml", "codegen.ml", "ast.ml"]},
    "genxml.ml"=>{:ml_deps=>["type.ml", "lexer.ml", "common.ml", "ast.ml"], :depends_on => l(:extlib)},
    "interp.ml"=>{:ml_deps=>["typecore.ml", "type.ml", "lexer.ml", "genneko.ml", "common.ml", "codegen.ml", "ast.ml", "genswf.ml", "parser.ml"], :depends_on => l(:extlib, :extc, :xml_light)},
    "matcher.ml"=>{:ml_deps=>["optimizer.ml", "codegen.ml", "typecore.ml", "type.ml", "typer.ml", "common.ml", "ast.ml"], :depends_on => l(:extlib)},
    "main.ml"=>{:ml_deps=>["dce.ml", "matcher.ml", "typer.ml", "typeload.ml", "typecore.ml", "type.ml", "parser.ml", "optimizer.ml", "lexer.ml", "interp.ml", "genxml.ml", "genswf.ml", "genphp.ml", "genneko.ml", "genjs.ml", "gencpp.ml", "genas3.ml", "common.ml", "codegen.ml", "ast.ml", "gencommon.ml", "genjava.ml", "gencs.ml"], :depends_on => l(:extlib, :extc)},
    "optimizer.ml"=>{:ml_deps=>["typecore.ml", "type.ml", "parser.ml", "common.ml", "ast.ml"], :depends_on => l(:extlib)},
    "parser.ml"=>{:ml_deps=>["lexer.ml", "common.ml", "ast.ml"], :depends_on => l(:extlib)},

    "type.ml"=>{:ml_deps=>["ast.ml"], :depends_on => l(:extc, :extlib) },
    "typecore.ml"=>{:ml_deps=>["type.ml", "common.ml", "ast.ml"], :depends_on =>l(:extlib) },
    "typeload.ml"=>{:ml_deps=>["typecore.ml", "type.ml", "parser.ml", "optimizer.ml", "lexer.ml", "common.ml", "ast.ml"], :depends_on => l(:extlib)},
    "typer.ml"=>{:ml_deps=>["typeload.ml", "typecore.ml", "type.ml", "parser.ml", "optimizer.ml", "lexer.ml", "interp.ml", "genneko.ml", "genjs.ml", "common.ml", "codegen.ml", "ast.ml"], :depends_on => l(:extlib)},
    "lexer.ml"=>{:ml_deps=>["ast.ml"]}

  }

  files["interp.ml"][:depends_on] += l(:neko) if ACTIVE_BACKENDS.include? :neko
  files["common.ml"][:depends_on] += l(:swflib) if ACTIVE_BACKENDS.include? :swf
  files["common.ml"][:depends_on] += l(:javalib) if ACTIVE_BACKENDS.include? :java
  files["genswf.ml"][:depends_on] = l(:ttflib)

  files["parser.ml"][:camlp4] = 'camlp4o'

  # create rake tasks:
  libs.each_pair {|k, v| v.tasks }

  haxe = OcamlExecutable.new({
    :alias_for_main_task => "haxe#{compiler.exe_suffix}",
    :patches => [HaxeBackends.new(ACTIVE_BACKENDS)] + $tracing,
    :base_dir => "./",
    :target_rel => "haxe#{compiler.exe_suffix}",
    :files => files,
    :compiler => compiler,
    :depends_on => l(:extc) + [OcamlLinkFlags.new(
      "unix.#{compiler.lib_ext}",
      "str.#{compiler.lib_ext}"
    )]
  }).tasks

end

My_CleanTask.new("clean_libs_my", $libs_to_clean.to_a)
task "clean_libs_make" => $libs_to_clean_tasks.to_a
task "clean_libs" => ["clean_libs_my", "clean_libs_make"]

My_FileTask.new({"run_tracetest" => ["tracetest"]}, ["./tracetest"])

$tasks.each {|v| v.rumake_task }

My_Task.new({"clean" => ["clean_haxe", "clean_libs"]}, []).rumake_task

# makefile rake task {{{ 1

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
  # currently broken
  #
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
  BACKEND_FILES.each_pair {|k,v|
    out.write("#{k == :neko ? "" : "# "}BACKEND_#{k}=yes")
  }
  out.write("")

  # HELP_TEXT.split("\n").each {|v|
  #   out.write("# #{v.gsub("drake", "make")}")
  # }
  out.write("")


  $tasks.each {|v| v.makefile(out) }

  makefile = out.string

  BACKEND_FILES.each_pair {|k,v|
    makefile = makefile.gsub("-D BACKEND_#{k}", "$(if ${BACKEND_#{k}}, -D BACKEND_#{k},)")

    v.each {|file|
      # mind the space, this does not drop rules at ^
      makefile = makefile.gsub(" #{file}.cmx", " $(if ${BACKEND_#{k}}, #{file}.cmx,)")
      makefile = makefile.gsub(" #{file}.cmo", " $(if ${BACKEND_#{k}}, #{file}.cmo,)")
    }
  }

  File.open('makefile-generated-by-rake', "wb") { |file| file.write(makefile) }
end

Rumake::TaskContainer.instance.init("rumake.cache", 4)
if ARGV[0] == "list"
  Rumake::TaskContainer.instance.list
else
  Rumake::TaskContainer.instance.realise(false, *ARGV)
end

# vim: fdm=marker
