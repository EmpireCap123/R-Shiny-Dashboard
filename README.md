# .arcconfig
{
  "phabricator.uri" : "https://phabricator.corp.stateauto.com/",
  "base": "git:merge-base(origin/dev)",
  "arc.land.onto.default": "dev",
  "arc.feature.start.default": "dev",
  "history.immutable": false
}

# .arclint
{
  "linters": {
    "spelling": {
      "type": "spelling"
    },

    "filename": {
      "type": "filename"
    },

    "text": {
      "type": "text"
    },

    "merge-conflict": {
      "type": "merge-conflict"
    },
    "pep8": {
      "type": "pep8",
      "include": "(\\.py$)"
    }
  }
}

# .gitignore
# Ignore everything
*

# But descend into directories
!*/

# And permit certain filetypes
!*.R
!*.Rmd
!*.md
!*.css
!*.txt
!*.js
!*.sql
!*.json
!*.sh
!*.book
!*.diviner
!*.html
!*.sas
!*.py
!*.ipynb
!*.sqlproj
!*.dbmdl
!*.sln
!*.xml
!*.bat
!*.dtsx
!*.dll
!*.database
!*.dtproj

# And dotfiles
!.gitignore
!.arclint
!.arcunit
!.arcconfig
