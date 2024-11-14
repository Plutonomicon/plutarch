{ inputs, ... }: {
  imports = [
    inputs.hercules-ci-effects.flakeModule
  ];

  hercules-ci.flake-update = {
    enable = true;
    updateBranch = "updated-flake-lock";
    # Next two parameters should always be set explicitly
    createPullRequest = true;
    autoMergeMethod = null;
    when = {
      # Perform update by Sundays at 12:45
      minute = 45;
      hour = 12;
      dayOfWeek = "Sun";
    };
  };

  herculesCI.ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
}
