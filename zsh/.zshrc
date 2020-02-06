# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
  export ZSH="/home/jkazan/.oh-my-zsh"

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
)

source $ZSH/oh-my-zsh.sh

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
export PATH=$PATH:/opt/apache-maven-3.6.0/bin

export JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
M2_HOME="/opt/apache-maven-3.6.0"


alias rpi_local='ssh -X pi@raspberrypi.local'
alias rpi='ssh -l pi proxy50.rt3.io -p 37918'
# pass = testavanligaochhassio
# ssh -l pi proxy55.rt3.io -p 38497
# sudo iwconfig wlan0 essid comhem_F00C83 key 8788B09B2D
# sudo iwconfig wlp6s0 essid jkazan key puws1111


# epics alias
# alias e3="bash /epics/test/base-7.0.1.1/require/3.0.2/bin/setE3Env.bash"
alias e3="source /home/jkazan/e3-aug06/base-7.0.3/require/3.1.0/bin/setE3Env.bash no_msg"
alias css="/opt/cs-studio/ESS\ CS-Studio"
alias e3get="~/e3-aug06/e3/caget_pvs.bash"
alias iocsh="/home/jkazan/e3-aug06/e3/e3-require/tools/iocsh.bash"

# sw-vm
alias swvm="ssh -X johanneskazantzidis@10.4.4.46"

#icsvd
alias icsvd="ssh -X johanneskazantzidis@icsvd-app01.esss.lu.se"

# Hermes
alias hermes="source ~/Hermes/bin/activate && python3 ~/Hermes/hermes/hermes/hermes.py"

# SC2
# alias sc2='env WINEPREFIX="/home/jkazan/.wine-hs-32" wine C:\\windows\\command\\start.exe /Unix /home/jkazan/.wine-hs-32/dosdevices/c:/users/Public/Desktop/StarCraft\ II.lnk'

# export NVM_LAZY_LOAD=true
alias ll="ls -al"
# export PATH=$PATH:/home/jkazan/e3-aug-06/base-7.0.3/bin/linux-x86_64/

alias selenium="source ~/martinkwick/bin/activate"

# Arduino
alias arduino="~/arduino-1.8.9/arduino"
# alias ardup="screen -S arduino_monitor -X quit && arduino --port /dev/ttyUSB* --upload"
alias ardup="if screen -S arduino_monitor -X quit; then echo 'killing monitor'; fi && arduino --port /dev/ttyUSB* --upload"
alias ardmon="screen -S arduino_monitor /dev/ttyUSB* 115200"

# Invert screen
alias invert="/usr/local/bin/xrandr-invert-colors"
