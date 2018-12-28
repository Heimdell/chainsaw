
-- | Simple demo, showing ability to make payments, pay fees and check nonces.
--

import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Catch

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Typeable

import Chainsaw.API

import Demo.Entities
import Demo.Monad
import Demo.Access
import Demo.Actions

run :: IO Status
run = do
    let tx1 = Provides (Author 2 0) $ PayFees $ CheckNonce [Pay 3 10, Pay 1 55]
    let tx2 = Provides (Author 2 1) $ PayFees $ CheckNonce [Pay 3 15, Pay 1 10]

    test $ do
        st <- get

        liftIO $ putStrLn $ "State:\n" ++ show (nest 4 $ pretty st)
        liftIO $ putStrLn ""
        liftIO $ putStrLn $ "Applying:\n" ++ show (nest 4 $ pretty [tx1, tx2])
        liftIO $ putStrLn ""

        (res, undoer) <- apply [tx1, tx2]

        st <- get

        liftIO $ putStrLn $ "Applied, state:\n" ++ show (nest 4 $ pretty st)
        liftIO $ putStrLn ""
        liftIO $ putStrLn $ "Undoing:\n" ++ show (nest 4 $ pretty undoer)
        liftIO $ putStrLn ""

        undo undoer

        st <- get

        liftIO $ putStrLn $ "Undone, state:\n" ++ show (nest 4 $ pretty st)
        return ()

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

main = run >>= print
