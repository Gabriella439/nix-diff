{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }: let
  system = "x86_64-linux";
  package = nixpkgs.lib.nixosSystem {
    inherit system;
    modules = [
      {
        boot.loader.grub.devices = [ "/dev/sda" ];
        
        fileSystems."/" = {
          device = "/dev/sda";
        };

        services.apache-kafka.enable = true;
      }
    ];
  };
  in {
    packages.${system}.default = package.config.system.build.toplevel;
  };
}
