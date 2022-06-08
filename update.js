const fs = require('fs')
const { exec } = require('child_process')

function logErr(err, branch) {
  console.error('\033[0;31m(' + branch + '): ' + err + '\033[0m')
}

function logInfo(s, branch) {
  console.log('\033[0;32m(' + branch + '): ' + s + '\033[0m')
}

fs.readFile('./.gitmodules', 'utf8', (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  
  // let matched = data.match(/path = (.+)/g) // 
  if (matched) {
    matched = matched.forEach(m => {
      const path = m.replace(/^path = /, '')

      const _c = branch => `cd \$HOME/.emacs.d/${path} && git reset --hard HEAD && git checkout ${branch} && git pull origin ${branch}`
      
      exec(_c('main'), (err, stdout, stderr) => {
        if (err) {
          exec(_c('master'), (err) => {
            if (err) {
              logErr(err, 'master')
            } else {
              logInfo(stdout, 'master')
            }
          })
        } else {
          logInfo(stdout, 'main')
        }
      })
    })        
  }
})

