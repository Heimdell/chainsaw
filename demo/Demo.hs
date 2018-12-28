
-- | Simple demo, showing ability to make payments, pay fees and check nonces.
--

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map

import Chainsaw.API

import Demo.Entities
import Demo.Monad
import Demo.Access ()
import Demo.Actions

run :: IO Status
run = do
    let tx1 = Provides (Author 2 0) $ PayFees $ CheckNonce [Pay 3 10, Pay 1 55]
    let tx2 = Provides (Author 2 1) $ PayFees $ CheckNonce [Pay 3 15, Pay 1 10]

    test $ do
        st0 <- get

        liftIO $ putStrLn $ "State:\n" ++ show (nest 4 $ pretty st0)
        liftIO $ putStrLn ""
        liftIO $ putStrLn $ "Applying:\n" ++ show (nest 4 $ pretty [tx1, tx2])
        liftIO $ putStrLn ""

        (_, undoer) <- apply [tx1, tx2]
        st1         <- get

        liftIO $ putStrLn $ "Applied, state:\n" ++ show (nest 4 $ pretty st1)
        liftIO $ putStrLn ""
        liftIO $ putStrLn $ "Undoing:\n" ++ show (nest 4 $ pretty undoer)
        liftIO $ putStrLn ""

        _   <- undo undoer
        st2 <- get

        liftIO $ putStrLn $ "Undone, state:\n" ++ show (nest 4 $ pretty st2)
        return ()

test :: M a -> IO Status
test action = do
    action
        `runReaderT` Env (Miner 1)
        `execStateT` Status
            { statusAccounts = Map.fromList
                [ (1, Account 0 100)
                , (2, Account 0 180)
                , (3, Account 0 50)
                ]
            , paymentFees = TheFees 0.1 10
            }

main :: IO ()
main = run >>= print
