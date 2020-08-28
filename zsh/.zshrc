# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:$HOME/bin/scripts:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/johannek/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="jkazan"
# sunaku
# philips
# minimal
# eastwood
# lambda

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    fzf
    zsh-autosuggestions
    # zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# Axis -------------------------------------------------------------------------
# Run tmux as default terminal
# if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#     exec tmux
# fi

# Export
export http_proxy=http://wwwproxy:3128
export https_proxy=http://wwwproxy:3128
export ftp_proxy=http://wwwproxy:3128

# Unset export
alias unset-proxy="unset http_proxy && unset https_proxy && unset ftp_proxy"

# Source flash_camera_functions.zsh
#source ~/bin/scripts/zshcustom/flash_camera_functions.zsh

# Aliases
# alias acap="source /opt/axis/acapsdk/3.01.0/cortexa9hf-neon/environment-setup-cortexa9hf-neon-poky-linux-gnueabi"
alias acap="source /opt/axis/acapsdk/3.00.1/cortexa9hf-neon/environment-setup-cortexa9hf-neon-poky-linux-gnueabi"
alias kaka="bash ~/bashscripts/kaka.sh"
# alias flash_camera="bash ~/bashscripts/axis_flash.sh"
alias dockerprod="docker run --net=host \
       --rm \
       --privileged \
       -v ~/axis/tools:/kpi_workspace/tools \
       -v ~/workspace/axis_object_detector:/kpi_workspace/axis_object_detector \
       -v ~/workspace/annotations:/kpi_workspace/annotations \
       -v ~/workspace/temp_vis:/kpi_workspace/temp_vis \
       -v ~/workspace/kpi_toolchain:/kpi_workspace/kpi_toolchain \
       -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
       -v /mnt/analyticsvideo:/analyticsvideo \
       -v /n/slask/johannek:/slask/johannek \
       -v ~/share:/share \
       -it -e DISPLAY=$DISPLAY \
       docker-prod.se.axis.com/sva/kpi-tool:1.2 bash"

alias rebuild_aoa='echo "$(tput bold)sourcing sdk...$(tput sgr0)" && source /opt/axis/acapsdk/2.12.0/environment-setup-cortexa9hf-neon-poky-linux-gnueabi
      echo "$(tput bold)going into workspace dir...$(tput sgr0)" && cd ~/axis/
      echo "$(tput bold)cleaning axis_object_detector...$(tput sgr0)" && sudo rm -rf axis_object_detector/
      echo "$(tput bold)cloning fresh aoa repo...$(tput sgr0)" && git clone ssh://johannek@gittools.se.axis.com:29418/apps/acap/axis_object_detector
      echo "$(tput bold)going into axis_object_detector/ directory...$(tput sgr0)" && cd axis_object_detector/
      echo "$(tput bold)updating git submodule...$(tput sgr0)" && git submodule update --init
      echo "$(tput bold)building...$(tput sgr0)" && ./build.py'

alias menu="~/lunch-menus/lunch_menus.sh"
alias tailc="~/bashscripts/tailc"

alias ll="lsa -t"

    # echo "\nsourcing sdk...\n\n" && source /opt/axis/acapsdk/2.12.0/environment-setup-cortexa9hf-neon-poky-linux-gnueabi \
    # echo "\ngoing into workspace dir...\n\n" && cd ~/workspace/ \
    # echo "\ncleaning axis_object_detector...\n\n" && sudo rm -rf axis_object_detector/
# echo "\ncloning fresh aoa repo...\n\n" && git clone ssh://johannek@gittools.se.axis.com:29418/apps/acap/axis_object_detector \
    # echo "\ngoing into axis_object_detector/ directory...\n\n" && cd axis_object_detector \
    # echo "\nupdating git submodule...\n\n" && git submodule update --init \
    # echo "\nbuilding...\n\n" && ./build.py"

# ------------------------------------------------------------------------------

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='emacs'
else
    export EDITOR='emacs'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Add to PATH: maven
# export PATH=$PATH:/opt/apache-maven-3.6.0/bin
# export JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
# M2_HOME="/opt/apache-maven-3.6.0"


alias rpi_local='ssh -X pi@raspberrypi.local'
alias rpi='ssh -l pi proxy50.rt3.io -p 37918'

# Hermes
alias hermes="source ~/hermes-env/bin/activate && python3 ~/hermes-env/hermes/hermes/hermes.py"

# export NVM_LAZY_LOAD=true
alias ll="ls -al"

# Invert screen
alias invert="/usr/local/bin/xrandr-invert-colors"

# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#47134f,bold"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# export ENHANCD_FILTER=fzf
# source ~/.oh-my-zsh/custom/plugins/enhancd/init.sh
