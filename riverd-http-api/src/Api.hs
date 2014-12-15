module Api where

import Rest.Api
import qualified Rest.Resource as R

import qualified Api.Project as Project
--import qualified Api.Dependency as Dependency


api :: Api IO
api = [(mkVersion 1 0 0, Some1 riverd)]

riverd :: Router IO IO
riverd =
    root -/ project
    where
        project     = route Project.resource


