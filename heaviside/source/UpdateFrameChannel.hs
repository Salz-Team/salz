module UpdateFrameChannel ( updateFrameChannel ) where

import qualified Network.AMQP  as RMQ

updateFrameChannel :: RMQ.Channel -> IO ()
updateFrameChannel chan = return ()

