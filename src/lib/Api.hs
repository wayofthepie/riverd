module Api where

import Rest.Api
import qualified Rest.Resource as R


import Api.Types.RiverdApi
import qualified Api.Project as Project
import qualified Api.Build   as Build
--import qualified Api.Dependency as Dependency


api :: Api RiverdApi
api = [(mkVersion 1 0 0, Some1 riverd)]

riverd :: Router RiverdApi RiverdApi
riverd =
    root -/ project
         -/ build
    where
        project = route Project.resource
        build   = route Build.resource

