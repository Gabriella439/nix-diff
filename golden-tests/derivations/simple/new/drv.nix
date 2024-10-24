let
  one = derivation {
    name = "soft_from_SW";
    builder = "Luke Skywalker";
    system = "C3PO";
    newEnvVaribale = "value";
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
    name = "new";
    builder = "builder";
    system = "x86_64-linux";
  };
  outputsMissmatch = derivation {
    name = "outputs";
    outputs = [ "lib" "headers" "doc" ];
    builder = "builder";
    system = "x86_64-linux";
  };
in
derivation {
  name = "drv";
  builder = "handmade";
  srcs = [ ./changed-file ./new-file ];
  derivations = [ two three namesMissmatch outputsMissmatch ];
  args = [ "one" "three" "four" ];
  system = "x86_64-linux";
}
