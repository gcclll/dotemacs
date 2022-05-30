const fs = require('fs')
const { execSync, exec } = require('child_process')

function logErr(err, branch) {
  console.error('\033[0;31m(' + branch + '): ' + err + '\033[0m')
}

function logInfo(s, branch) {
  console.log('\033[0;32m(' + branch + '): ' + s + '\033[0m')
}

fs.readFile('./bak.gitmodules', 'utf8', (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  
  let matched = data.match(/\[submodule[^[]+/g)
  if (matched) {
    matched.forEach((m,i) => {
      //if (i > 1) return
      const path = m.match(/path = ([^\n]+)/)[1]
      const url = m.match(/url = ([^\n]+)/)[1]
      if (fs.existsSync(`/Users/gcl/.emacs.d/${path}`)) {
        console.log(`~/.emacs.d/${path} existed.`)
        return
      }
      exec(`git submodule add ${url} ${path}`, (err, stdout, stdin) => {
        if (err) {
          //console.log('\nERROR: ' + err + '\n')
          return
        }
      })
    })
  }
})

