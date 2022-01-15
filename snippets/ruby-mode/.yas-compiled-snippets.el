;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
                     '(("yds" "# ============================================================================\n# $0PRIVATE INSTANCE METHODS\n# ============================================================================" "yds" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/yds" nil nil)
                       ("ydm" "# [Description goes here]\n#\n# @author Matt Sears\n#\n# @see http://example.com Description of URL\n# @see SomeOtherClass#method\n#\n# @private\n#\n# @param [Hash] opts the options to create a message with.\n# @option opts [String] :subject The subject\n# @option opts [String] :from ('nobody') From address\n#\n# The options parsed out of the commandline.\n# Default options are:\n#   :format => :dot\n#\n# @example Parse a glob of files\n#   YARD.parse('lib/**/*.rb')\n#\n# @raise [ExceptionClass] description\n#\n# @return [true] always returns true\n# @return description here with no types" "rdm" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/ydm" nil nil)
                       ("ydc" "# frozen_string_literal: true\n\n# Description goes here\n#\n# @author Matt Sears\n# @abstract\n# @since 0.1.0\n#" "rdc" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/ydc" nil nil)
                       ("ydb" "# for block {|a, b, c| ... }\n# @yield [a, b, c] Description of block\n#\n# @yieldparam [optional, types, ...] argname description\n# @yieldreturn [optional, types, ...] description" "rdb" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/ydb" nil nil)
                       ("yda" "# @todo Add support for Jabberwocky service\n#   There is an open source Jabberwocky library available\n#   at http://somesite.com that can be integrated easily\n#   into the project." "rda" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/yda" nil nil)
                       ("tds" "# ============================================================================\n# $0PUBLIC CLASS METHODS\n# ============================================================================" "tds" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/tds" nil nil)
                       ("tdm" "# Public: $0\n#\n# text  - The String to be duplicated.\n# count - The Integer number of times to duplicate the text.\n#\n# Examples\n#\n#   multiplex('Tom', 4)\n#   # => 'TomTomTomTom'\n#\n# Returns the duplicated String." "tdm" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/tdm" nil nil)
                       ("tdc" "# frozen_string_literal: true\n\n# Public: $0\n#\n# Examples\n#\n#   Math.square_root(9)\n#   # => 3" "tdc" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/tdc" nil nil)
                       ("tda" "# Public: $0" "tda" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/tda" nil nil)
                       ("she" "#!/usr/bin/env ruby$0" "shebang" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/shebang" nil nil)
                       ("rti" "# frozen_string_literal: true\n\n# See UsersController\nclass UsersControllerTest < ActionDispatch::IntegrationTest\n  let(:user) { create(:user) }\n\n  before { sign_in(user) }\n\n  describe \"updating bookmarks\" do\n    before do\n      put users_path(user), params: { user: { name: \"Test User\" } }, as: :turbo_stream\n    end\n\n    it \"return a successful response\" do\n      assert_response :success\n    end\n\n    it \"renders the correct template\" do\n      assert_template \"users/update\"\n    end\n\n    it \"something\" do\n      assert_equal user, assigns(:user)\n    end\n  end\n\n  describe \"get users\" do\n    before { get users_path, params: { user_id: user.id } }\n\n    it \"should return a sucessful response\" do\n      assert_response :success\n    end\n  end\nend\n" "rti" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/rti" nil nil)
                       ("rtc" "# frozen_string_literal: true\n\nrequire \"test_helper\"\n\n# See User\nclass UserTest < ActiveSupport::TestCase\n\n  let(:user) { create(:user) }\n\n  subject do\n    Users::Update.run(user: user, name: \"Test Case\")\n  end\n\n  describe \"saves users data\" do\n    before { subject.result }\n\n    it \"command should return true\" do\n      assert subject.success?\n    end\n\n    it \"should return the new collection\" do\n      assert User.exists?(id: user.id, name: \"Test Case\")\n    end\n  end\nend" "rtc" nil
                        ("ruby")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/rtc" nil nil)
                       ("req" "require '$0'" "require \"...\"" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/req" nil nil)
                       ("rel" "require_relative '$0'" "require_relative" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/rel" nil nil)
                       ("pryr" "binding.pry_remote" "pryr \"...\"" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/pryr" nil nil)
                       ("pry" "binding.pry" "pry \"...\"" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/pry" nil nil)
                       ("map" "map { |${e}| $0 }" "map { |...| ... }" nil
                        ("collections")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/map" nil nil)
                       ("let" "let(:$1) { $0 }\n" "let" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/let" nil nil)
                       ("it" "it '$0' do\n\nend" "it" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/it" nil nil)
                       ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil
                        ("collections")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/inject" nil nil)
                       ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil
                        ("control structure")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/ife" nil nil)
                       ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil
                        ("control structure")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/if" nil nil)
                       ("fb" "# frozen_string_literal: true\n\nFactoryBot.define do\n  factory :user do\n    first_name { \"John\" }\n    last_name  { \"Doe\" }\n    sequence(:username) { |n| \"user#{n}\" }\n    admin { false }\n\n    factory :sample_user do\n      first_name { FFaker::Name.first_name }\n    end\n\n  end\nend\n\ndef user_with_posts(posts_count: 5)\n  FactoryBot.create(:user) do |user|\n    FactoryBot.create_list(:post, posts_count, user: user)\n  end\nend\n" "fb" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/fb" nil nil)
                       ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil
                        ("collections")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/ea" nil nil)
                       ("desc" "describe '$0' do\n\nend" "desc" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/desc" nil nil)
                       ("def" "def ${1:method}${2:(${3:args})}\n    $0\nend" "def ... end" nil nil nil "/Users/matt/.emacs.d/snippets/ruby-mode/def" nil nil)
                       ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n				 (or (buffer-file-name)\n				     (buffer-name (current-buffer))))))))\n             (replace-regexp-in-string \"_\" \"\" fn t t))`}\n  $0\nend\n" "class ... end" nil
                        ("definitions")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/cls" nil nil)
                       ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/case" nil nil)
                       ("by" "byebug\n$0\n" "byebug" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/by" nil nil)
                       ("begin" "begin do\n  $0\nend" "begin" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/begin" nil nil)
                       ("before" "before do\n  $0\nend" "before" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/before" nil nil)
                       ("ba" "before_action :$0" "before_action" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/ba" nil nil)
                       ("%w" "%w($0)" "%w" nil
                        ("general")
                        nil "/Users/matt/.emacs.d/snippets/ruby-mode/%w" nil nil)))


;;; Do not edit! File generated at Fri Dec 17 15:11:50 2021
