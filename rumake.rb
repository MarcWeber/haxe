# encoding: UTF-8

require_relative "rumake/lib/rumake/task.rb"

module Rumake
  class Task
    alias :init :initialize
    def initialize(opts, &blk)
      opts[:prereqs] = opts.fetch(:prereqs, []).select {|v| v.respond_to? :main_target}.map {|v| v.main_target }
      init(opts, &blk)
    end

    def main_target; name; end
  end
end

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
#
# TODO: mark cross platform issues to be resolved by CROSS (mainly clean tasks,
# checkout tasks etc)


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
    @o[:depends_on] ||= []
    @o[:target_rel] = @o[:target_rel].gsub(/\LIB_EXT$/, cc.lib_ext)
    @o[:target_absolute] = File.join(@o[:base_dir], @o[:target_rel])
    @name = File.basename(@o[:base_dir])
  end

  def ocaml_cflags; ["-I", File.absolute_path(@o[:base_dir])]; end
  def ocaml_link_flags; o.fetch(:lib_flags, []) + [main_target]; end 

  def main_target; @o[:target_absolute]; end

  def tasks
    once clean_task do
      Rumake::Task.new({
        :aliases => clean_task,
        :shell_commands => ["make -C libs/#{@name} clean"],
        :prerqs => @o[:depends_on].main_targets
       })
    end

    make_args = (@o.fetch(:make_args_by_compiler_type, {}).fetch(@o[:compiler].type, []))
    # TODO refactor?, make it an configuration option
    aliases = []
    once @name.to_sym do
      aliases = [@name.to_sym]
    end

    Rumake::Tasks::File.new({
      :files => [main_target],
      :aliases => aliases,
      :prerqs => ["libs"] + @o[:depends_on],
      :shell_commands => 
        [ "make -C libs/#{@name} #{@o[:compiler].type == "native" ? "native" : "bytecode"}",
          # we track more dependencies than the makefile, so the makefile may not
          # touch the library file causing rebuilds again and again, so update the
          # timestamp
          "touch #{main_target}"
        ]
    })


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

    includes     = o.join_array(:includes)
    dependencies = o.join_array(:dependencies).flatten(1)

    pre_flags = ""
    pre_flags = ""

    cmd = case action
    when :compile_ml
      in_ = o.first(:in)
      pre = ""


      cppo_executable_fun = o.first(:cppo_executable_fun)

      # cppo
      cppo_flags = o.join_array(:cppo_flags)
      use_cppo = (o.values(:use_cppo).detect {|v| !!v}) || (cppo_flags.length > 0)

      # camlp4
      camlp4_flags = o.join_array(:camlp4_flags)
      camlp4_suggestions = o.values(:camlp4) || ['camlp4']
      raise "multiple camlp4 suggestions #{camlp4_suggestions}" if camlp4_suggestions.length > 1
      camlp4 = camlp4_suggestions.first

      use_camlp4 = camlp4
      # you can uncomment the hack above and comment the cppo line below
      cmd_cppo = \
       use_cppo \
       ? "#{cppo_executable_fun.call} #{cppo_flags.join(' ')}" \
       : nil

      cmd_camlp4 = 
        use_camlp4 \
        ? "#{camlp4} #{camlp4_flags.join(' ')}" \
        : nil

      pp = if use_cppo && use_camlp4
            # eventually provide patch for cppo adding a -camlp4 option making it do
            # this Requires tempfile, would requires adding new dependencies
            "-pp 'ruby trace/cppo_then_camlp4.rb #{cmd_cppo} -- #{cmd_camlp4}'"
      elsif use_cppo;   "-pp '#{cmd_cppo}'"
      elsif use_camlp4; "-pp '#{cmd_camlp4}'"
      else ""
      end

      [ 
      pre,
      cc,
      libs_cflags,
      includes.map {|v| "-I #{v}"},
      pp,
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
  def main_targets;
    self.select {|v| v.respond_to? :main_target}.map {|v| v.main_target}
  end
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
    Hash[self.map {|k, list| [k, list.main_targets ] }]
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

# provides makefile implementation for My_FileTask My_Task

class My_CleanTask
  include Rumake

  def initialize(name, dir, files = [])
    @name = name
    @files = files
    @dirs = dir
    @dirs = [@dirs] unless @dirs.is_a? Array
  end

  def rumake_task
    #@dirs.each {|dir| clean_dir dir }
    # out.write()
    Rumake::Task.new({
      :aliases => @name,
      :shell_commands => @dirs.map {|dir| "cd #{dir}; rm #{EXTS_TO_CLEAN.join(' ')} #{@files.join(' ')} || true\n" },
      :phony => true
    })
  end

  def makefile(out)
    out.write("#{@name}:\n")
    @dirs.each {|dir|
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
      # file_o[:camlp4] = 'camlp4o'
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
    o[:files].each_pair do |file, file_o|
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
      src = "#{o[:base_dir]}/#{file}"
      if file_o[:ocamllex]
        src = "#{o[:base_dir]}/#{file}l"
        once file do
          Rumake::Tasks::File.new({
            :files => ["#{o[:base_dir]}/#{file}"],
            :prereqs => ["#{o[:base_dir]}/#{file}l"],
            :shell_commands => ["cd #{o[:base_dir]}; ocamllex #{file}l"]
          })
        end
      end
      if file_o[:ocamlyacc]
        src = "#{o[:base_dir]}/#{file}y"
        once file do
          # ocamlyacc creates .mli and a .ml file
          Rumake::Tasks::File.new({
            :files => ["#{o[:base_dir]}/#{file}", "#{o[:base_dir]}/#{file}i" ],
            :prereqs => ["#{o[:base_dir]}/#{file}y"],
            :shell_commands => ["cd #{o[:base_dir]}; ocamlyacc #{file}y"]
          })
        end
      end

      once src do
        # eg make main.ml depend on .git checkout
        Rumake::Task.new({
          :aliases => src,
          :prereqs => o[:depends_on].main_targets
        })
      end

      case file
      when /\.ml$/
        ml_deps = (o[:files_sorted] && file_o[:ml_deps]).map {|v| File.join(o[:base_dir], cc.ml_mod_ext(v) ) }
        cmd = "cd #{o[:base_dir]}; #{cc.cc_compile({:cppo_executable_fun => cppo_executable_fun,:out => file_o[:out_rel], :depends_on => depends_on, :in => file}, annot, o, file_o)}"

        Rumake::Tasks::File.new({
          :files => [file_o[:out_absolute]],
          :prereqs => cppo_dep + [file_o[:absolute_path]] + ml_deps + depends_on,
          :shell_commands => [cmd]
        })
        mls_for_target << file
      when /\.mli$/
        once file_o[:out_absolute] do
          ml_deps = (o[:files_sorted] && file_o[:ml_deps]).map {|v| File.join(o[:base_dir], cc.ml_mod_ext(v) ) }
          cmd = "cd #{o[:base_dir]}; #{cc.cc_compile({:cppo_executable_fun => cppo_executable_fun, :out => file_o[:out_rel], :depends_on => depends_on, :in => file}, o, file_o)}"


          Rumake::Tasks::File.new({
            :files => file_o[:out_absolute],
            :prereqs => cppo_dep + [File.join("./", file_o[:absolute_path])] + ml_deps + depends_on,
            :shell_commands => [cmd]
          })
        end
      when /\.c$/
        once file_o[:out_absolute] do
          cmd = "cd #{o[:base_dir]}; ocamlc #{file_o.fetch(:CFLAGS, []).join(' ')} #{file}"
          Rumake::Tasks::File.new({
            :files => file_o[:out_absolute].gsub(/\.c$/, '.o'),
            :prereqs => [file_o[:absolute_path]] + o[:depends_on],
            :shell_commands => [cmd]
          })
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
    end

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

    Rumake::Tasks::File.new({
      :files => out_absolute,
      :aliases => @o.fetch(:alias_for_main_task, []),
      :prereqs => target_depends_on + o[:files].map {|k,v| v[:out_absolute]},
      :shell_commands => [cmd]
    })

    if not @o[:alias_for_main_task].nil?
      once "clean_#{@o[:alias_for_main_task]}" do
        My_CleanTask.new("clean_#{@o[:alias_for_main_task]}", o[:base_dir]).rumake_task
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
    @extra_targets << {:files => files, :cmd => cmd, :depends_on_targets => [main_target] + depends_on_targets}
    self
  end

  def main_target
    @path
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
        branch = (@repo.include? :branch) ? "-b #{@repo[:branch]}" : ""
        cmds << "git clone #{@repo[:url]} #{branch} #{@path}"
      else raise "not implemented #{@repo[:type]}"
      end
    Rumake::Tasks::Dir.new(
      :dirs => ["#{@path}"],
      :shell_commands => cmds
    )

    # compile etc
    @extra_targets.each {|v|
      Rumake::Tasks::File.new({
        :files => v[:files],
        :prereqs =>  v[:depends_on_targets],
        :shell_commands => [*v[:cmd]]
      })
    }
    if @clean_command
      # TODO ?
      Rumake::Task.new({
        :aliases => "clean_#{@path}",
        :prereqs => @clean_command
      })
    end
  end
end

# Repository.new("libs", {:url => 'git://github.com/HaxeFoundation/ocamllibs.git'})
# waiting for https://github.com/HaxeFoundation/ocamllibs/pull/3
libs_checkout = Repository.new("libs", {:url => 'git://github.com/MarcWeber/ocamllibs.git'})

# arbitrarely choosing "doc" as directory which must exist to checkout haxe code
haxe_checkout = Rumake::Tasks::Dir.new({
  :dirs => ".git",
  :shell_commands => 'git clone -b t/rake-conditional-compilation git://github.com/MarcWeber/haxe.git haxe-checkout-tmp; rmdir haxe-checkout-tmp/libs; mv haxe-checkout-tmp/.* haxe-checkout-tmp/* .; rmdir haxe-checkout-tmp'
})

# good idea to use cppo github url? Or release? Let's hope its stable
Repository.new("cppo", {:url => 'https://github.com/mjambon/cppo.git'}) \
          .add_target(["cppo/cppo"], ["make -C cppo"]) \
          .clean_command(["make -C cppo clean"])

# Repository.new("deriving", {:url => "git://github.com/jaked/deriving.git"}, Dir["deriving/**/*.ml", "deriving/**/*.mli"]) \
#           .add_target(["deriving/syntax/deriving"], ["make -C deriving"]) \
#           .clean_command(["make -C deriving clean"])

# Repository.new('extunix', {:url => 'https://forge.ocamlcore.org/frs/download.php/1146/ocaml-extunix-0.0.6.tar.gz', :type => :tar_gz, :rename => ['ocaml-extunix-0.0.6', 'extunix']} )
#           .add_target(["extunix/_build/src/extunix.LEXT"], ["cd extunix; ./configure && make"]) \


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
      v[:camlp4] ||= 'camlp4o'
      v[:depends_on] ||= []
      v[:depends_on] << "trace/instr.cmo"
    }
  end
end

# luckily I do no longer need this now
# $tracing = [Tracing.new]
$tracing = []

$libs_to_clean_tasks = Set.new
$libs_to_clean = Set.new

%w{trace Camlp4Tracer instr}.each {|v|
  Rumake::Tasks::File.new({
    :files => "trace/#{v}.cmo",
    :prereqs => ["trace/#{v}.ml"],
    :shell_commands => [ "cd trace; ocamlc -dtypes -pp 'camlp4r -I +camlp4 -parser Camlp4QuotationCommon  -parser Camlp4QuotationExpander' -I +camlp4 -o #{v}.cma -c #{v}.ml" ]
  })
}
$repositories.each_pair {|k,r| r.tasks }

[Ocamlopt.new, OcamlC.new] .each do |compiler|

  OcamlExecutable.new({
    :alias_for_main_task => "tracetest#{compiler.exe_suffix}",
    :patches => $tracing,
    :base_dir => "trace",
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
  :depends_on => [libs_checkout],
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
  :depends_on => [libs_checkout],
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
  :depends_on => [libs_checkout],
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
  :depends_on => [libs_checkout],
})
libs[:ziplib] = LibMake.new({
  :base_dir => "libs/ziplib",
  :compiler => compiler,
  :target_rel => "zip.LIB_EXT",
  :alias_for_main_task => "ziplib.#{compiler.type}",
  :depends_on => [libs_checkout],
})
libs[:swflib] = LibMake.new({
  :base_dir => "libs/swflib",
  :compiler => compiler,
  :target_rel =>  "swflib.LIB_EXT",
  :alias_for_main_task => "swflib.#{compiler.type}",
  :depends_on => [libs_checkout],
})

if true
libs[:xml_light] = LibMake.new({
    :base_dir => "libs/xml-light", 
    :compiler => compiler,
    :target_rel => "xml-light.LIB_EXT", 
    :alias_for_main_task => "xml-light.#{compiler.type}",
    :make_args_by_compiler_type => {"native" => "xml-light.cmxa", "bytecode" => "xml-light.cma"},
    :depends_on => [libs_checkout],
})
else

libs[:xml_light] = OcamlLib.new(
  :base_dir => "libs/xml-light",
  :compiler => compiler,
  :target_rel => "xml-light.LIB_EXT",
  :alias_for_main_task => "xml-light.#{compiler.type}",
  :patches => lib_tracing,
  :depends_on => [libs_checkout],
  :files => {

    "dtd.mli" => {},
    "xml.mli" => {},
    "dtd.mli" => {:ml_deps => %w{xml.mli}},
    "xmlParser.mli" => {:ml_deps => %w{dtd.mli xml.mli}},
    "xml_parser.mli" => {:ml_deps => %w{dtd.mli}},
    "xml_lexer.mli" => {:ml_deps => %w{dtd.mli}},


    "dtd.ml" => {:ml_deps => %w{xml.mli xml_lexer.mli dtd.mli}},
    "xml.ml" => {:ml_deps => %w{dtd.mli xmlParser.mli xml_lexer.mli xml.mli}},
    "xmlParser.ml" => {:ml_deps => %w{dtd.mli xml.mli xml_lexer.mli xmlParser.mli}},
    "xml_parser.ml" => {:ocamlyacc => true, :ml_deps => %w{dtd.mli xml_parser.mli xml_parser.mli}},
    "xml_lexer.ml" => {:ocamllex => true, :ml_deps => %w{xml_lexer.mli}},
  })
end

libs[:ttflib] = LibMake.new({
  :base_dir => "libs/ttflib", 
  :compiler => compiler,
  :alias_for_main_task => "ttflib.#{compiler.type}",
  :target_rel => "ttf.LIB_EXT",
  :depends_on => [libs_checkout],
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
    "lexer.ml"=>{:ocamllex => true, :ml_deps=>["ast.ml"]}

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
    :base_dir => ".",
    :target_rel => "haxe#{compiler.exe_suffix}",
    :files => files,
    :compiler => compiler,
    :depends_on => l(:extc) + [haxe_checkout, OcamlLinkFlags.new(
      "unix.#{compiler.lib_ext}",
      "str.#{compiler.lib_ext}"
    )]
  }).tasks

end


Rumake::Task.new({ :aliases => "clean_libs_my"  , :prereqs => $libs_to_clean.to_a })
Rumake::Task.new({ :aliases => "clean_libs_make", :prereqs => $libs_to_clean_tasks.to_a})

Rumake::Task.new({ :aliases => "clean_libs", :prereqs => ["clean_libs_my", "clean_libs_make"] })

Rumake::Task.new({
  :aliases => "clean",
  :prereqs => ["clean_haxe", "clean_libs"]
})

# makefile rake task {{{ 1

Rumake::Tasks::File.new({
  :files => "makefile-inlined",
  :phony => true
  }) do
  # currently broken

  require_relative "rumake/lib/rumake/makefile.rb"

  errors = []
  out = "
# this makefile was created by Rakefile
# TODO: don't depend on libraries if backends are dropped
# you always have to enable BACKEND_neko
#
# important targets:
# haxe: creates haxe.native and haxe.bytecode
# haxe_clean: cleans haxe (has to be run if you enable disable backends,
# because timestamps don't change..
".split("\n")

  out << "# BACKEND_X=yes: comment to disable backend"
  BACKEND_FILES.each_pair {|k,v|
    out << "#{k == :neko ? "" : "# "}BACKEND_#{k}=yes"
  }
  out << ""

  # HELP_TEXT.split("\n").each {|v|
  #   out.write("# #{v.gsub("drake", "make")}")
  # }
  out << ""

  Rumake::TaskContainer.instance.makefile(out, errors)
  puts "WARNING: errors while creating makefile:"
  puts errors

  makefile = out.join("\n")

  BACKEND_FILES.each_pair {|k,v|
    makefile = makefile.gsub("-D BACKEND_#{k}", "$(if ${BACKEND_#{k}}, -D BACKEND_#{k},)")

    v.each {|file|
      # mind the space, this does not drop rules at ^
      makefile = makefile.gsub(" #{file}.cmx", " $(if ${BACKEND_#{k}}, #{file}.cmx,)")
      makefile = makefile.gsub(" #{file}.cmo", " $(if ${BACKEND_#{k}}, #{file}.cmo,)")
      makefile = makefile.gsub(Dir.pwd, '${CURDIR}')
    }
  }

  File.open('makefile-inlined', "wb") { |file| file.write(makefile) }
end

Rumake::TaskContainer.instance.init("rumake.cache", 4)
if ARGV[0] == "list"
  Rumake::TaskContainer.instance.list
else
  Rumake::TaskContainer.instance.realise({
    :show_eta => true,
    :targets => ARGV
  })
end

# vim: fdm=marker
