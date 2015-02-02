module Api where

import Rest.Api
import qualified Rest.Resource as R


import Api.Types.RiverdApi
import qualified Api.Project as Project
--import qualified Api.Dependency as Dependency


api :: Api RiverdApi
api = [(mkVersion 1 0 0, Some1 riverd)]

riverd :: Router RiverdApi RiverdApi
riverd =
    root -/ project
    where
        project = route Project.resource


