provision = "
apt install -y cabal-install vim ghc
sudo -u vagrant cabal update
sudo -u vagrant cabal install QIO
"

Vagrant.configure("2") do |config|
  config.vm.define "d" do |node|
    node.vm.box = "debian/stretch64"
    config.vm.synced_folder ".", "/vagrant", type: "sshfs"
    node.vm.provision "shell", inline: provision
    node.vm.provider "virtualbox" do |v|
      v.memory = 4096
    end
  end
end
