
Gem::Specification.new do |s|
  s.name = 'capitest'
  s.version = '1.0'

  s.summary = 'A simple test of Ruby C API'
  s.description = s.summary * 10
  
  s.authors = ['Łukasz Hanuszczak']
  s.homepage = 'https://github.com/mrhania'
  s.email = 'mrhania'
  
  s.files = [
    'lib/capitest.rb',
    'ext/capitest/extconf.rb',
    'ext/capitest/capitest.h',
    'ext/capitest/capitest.cpp'
  ]
  s.extensions = [
    'ext/capitest/extconf.rb'
  ]
end
