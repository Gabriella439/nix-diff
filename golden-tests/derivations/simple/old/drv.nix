let
  one = derivation {
    name = "soft_from_SW";
    builder = "Anakin Skywalker";
    system = "R2D2_astrodroid";
    missingEnvVaribale = "value";
  };
  two = derivation {
    name = "second_derivation";
    derivations = [ one ];
    builder = "builder";
    system = "x86_64-linux";
  };
  three = derivation {
    name = "third_derivation";
    derivations = [ one ];
    builder = "builder";
    system = "x86_64-linux";
  };
  namesMissmatch = derivation {
    name = "old";
    builder = "builder";
    system = "x86_64-linux";
  };
  outputsMissmatch = derivation {
    name = "outputs";
    outputs = [ "out" "bin" ];
    builder = "builder";
    system = "x86_64-linux";
  };
in
derivation {
  name = "drv";
  builder = "handmade";
  srcs = [ ./changed-file ./missing-file ];
  derivations = [ two three namesMissmatch outputsMissmatch ];
  args = [ "one" "two" "three" ];
  system = "x86_64-linux";
}
