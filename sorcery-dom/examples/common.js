let path = require('path')

module.exports.watchDirectories = (dir) => [
  path.resolve(dir, 'src'),
  path.resolve(dir, '..', '..', '..', 'sorcery', 'src'),
  path.resolve(dir, '..', '..', '..', 'sorcery-macros', 'src'),
  path.resolve(dir, '..', '..', '..', 'sorcery-dom', 'src'),
]
