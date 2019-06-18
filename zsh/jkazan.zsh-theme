# Git-centric variation of the "sunaku" theme.
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg_bold[green]%}+"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg_bold[magenta]%}!"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg_bold[red]%}-"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg_bold[blue]%}>"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg_bold[cyan]%}#"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[yellow]%}?"

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=" "
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

local user_color='208'
test $UID -eq 0 && user_color='red'

PROMPT='%(?..%{$fg_bold[red]%}exit %?
%{$reset_color%})'\
'%{$FG[111]%}$(git_prompt_info)%{$reset_color%}'\
'%(!.#.)'

zle_highlight=(default:bold)

# RPROMPT='%*'
RPROMPT='%B%{$FG[$user_color]%}%~%{$reset_color%}%b'\

PROMPT2='%{$fg[red]%}\ %{$reset_color%}'

# Put this line after line 18 to see git info
# '$(git_prompt_status)%{$reset_color%}'\
