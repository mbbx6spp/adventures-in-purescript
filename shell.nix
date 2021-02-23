{ pkgs ? import ./nixpkgs.nix
, presenterMode ? builtins.getEnv "MBBX6SPP_PRESENTER" }:
let
  inherit (pkgs) mkShell spago purescript aws-sam-cli nodejs-14_x entr texlive pdfpc pythonPackages graphviz beamerpresenter;
  texliveEnv = texlive.combine {
    inherit (texlive)
      beamer beamertheme-metropolis pgf pgfopts pdfpages
      listings listing listings-ext collection-fontsrecommended
      collection-mathscience collection-xetex fancyvrb fontspec caption
      tikz-cd fira etoolbox trimspaces environ ulem capt-of wrapfig tcolorbox
      booktabs translator minted fvextra upquote lineno xstring framed float
      beamertheme-focus beamerdarkthemes beamercolorthemeowl adjustbox xkeyval
      collectbox metalogo fontawesome catchfile emoji;
  };
  presenterTools = [
    texliveEnv
    pdfpc
    pythonPackages.pygments
    graphviz
    beamerpresenter
  ];
in mkShell {
  buildInputs = [
    nodejs-14_x
    spago
    purescript
    entr
  ] ++ (if presenterMode == "true" then presenterTools else []);
}
