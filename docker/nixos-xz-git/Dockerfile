FROM nixos/nix

# Compression utils
RUN  nix-channel --add https://channels.nixos.org/nixos-20.09 nixos \
  && nix-channel --update \
  && nix-env -iA nixos.git \
  && nix-env -iA nixos.gnutar \
  && nix-env -iA nixos.lzma \
  && nix-env -iA nixos.zstd \
  && nix-env -iA nixos.bash
