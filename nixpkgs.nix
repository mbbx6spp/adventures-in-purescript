import ((import <nixpkgs> { }).fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "2f304a0bafc38c5cde695fc91ded700b1f668ce6";
    sha256 = "026r951mmiw446q5m4jgx98icz97inv44khkg9dkk0hfxy78hj62";
}) {
  config.allowUnfree = true;
}
