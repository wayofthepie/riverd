module GitAnalyzer where


test = do
    let repoOpts = RepositoryOptions { repoPath = "/opt/repos/riverd"
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    repo <- openRepository repoOpts False
