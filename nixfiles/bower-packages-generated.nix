{ fetchbower, buildEnv }:
buildEnv { name = "bower-env"; ignoreCollisions = true; paths = [
  (fetchbower "tipsy" "0.1.7" "*" "0ldkxrl2vklw40afji09kmq1qqw71w1df77fp0bsv8ndnhnrdhr8")
  (fetchbower "foundation" "5.5.3" "*" "0ilwi48vn69rc117piqnkqb16l97nkn72jghiiznf6hqvwnbscx2")
  (fetchbower "jquery" "2.1.4" "*" "1hj2a4dgpgihxzg3f944gy77yqpfrj6213ilf5hjaa7cpzr1qz7i")
  (fetchbower "fontawesome" "4.4.0" "*" "0jy78jsyd705ccvmqgj9c78qi4jp6qivhkmn0yxdjqskf0243qzh")
]; }
