onNothing :: Maybe a -> IO () -> IO (Maybe a)
onNothing Nothing p = p >> (pure Nothing)
onNothing a _ = pure a


maybeRead = fmap fst . listToMaybe . reads
