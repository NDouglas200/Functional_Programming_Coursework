-- 
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

import Data.List
import Data.Char

type Title = String
type Artist = String
type Year = Int
type Sales = Int 

type Album = (Title, Artist, Year, Sales)

testData :: [Album]
testData = [("Greatest Hits", "Queen", 1981, 6300000),
    ("Gold: Greatest Hits", "ABBA", 1992, 5400000),
    ("Sgt. Pepper's Lonely Hearts Club Band", "The Beatles", 1967, 5340000),
    ("21", "Adele", 2011, 5110000),
    ("(What's the Story) Morning Glory?", "Oasis", 1995, 4940000),
    ("Thriller", "Michael Jackson", 1982, 4470000),
    ("The Dark Side of the Moon", "Pink Floyd", 1973, 4470000),
    ("Brothers in Arms", "Dire Straits", 1985, 4350000),
    ("Bad", "Michael Jackson", 1987, 4140000),
    ("Rumours", "Fleetwood Mac", 1977, 4090000), 
    ("Greatest Hits II", "Queen", 1991, 3990000), 
    ("Back to Black", "Amy Winehouse", 2006, 3940000), 
    ("The Immaculate Collection", "Madonna", 1990, 3700000),   
    ("25", "Adele", 2015, 3500000),   
    ("Stars", "Simply Red", 1991, 3450000), 
    ("Come On Over", "Shania Twain", 1998, 3430000),
    ("x", "Ed Sheeran", 2014, 3380000), 
    ("Legend", "Bob Marley", 1984, 3380000), 
    ("Bat Out of Hell", "Meat Loaf", 1977, 3370000), 
    ("Back to Bedlam", "James Blunt", 2004, 3360000), 
    ("Urban Hymns", "The Verve", 1997, 3340000), 
    ("Bridge over Troubled Water", "Simon & Garfunkel", 1970, 3260000), 
    ("1", "The Beatles", 2000, 3230000), 
    ("Spirit", "Leona Lewis", 2007, 3170000),  
    ("Crazy Love", "Michael Bublé", 2009, 3130000),  
    ("No Angel", "Dido", 2000, 3090000), 
    ("White Ladder", "David Gray", 1998, 3020000), 
    ("The Fame", "Lady Gaga", 2009, 2990000), 
    ("Only by the Night", "Kings of Leon", 2008, 2980000),   
    ("A Rush of Blood to the Head", "Coldplay", 2002, 2960000), 
    ("Talk on Corners", "The Corrs", 1997, 2960000), 
    ("Spice", "Spice Girls", 1996, 2960000),  
    ("Life for Rent", "Dido", 2003, 2900000), 
    ("Beautiful World", "Take That", 2006, 2880000), 
    ("The Joshua Tree", "U2", 1987, 2880000),  
    ("Hopes and Fears", "Keane", 2004, 2860000),
    ("The War of the Worlds", "Jeff Wayne", 1978, 2800000),
    ("X&Y", "Coldplay", 2005, 2790000),
    ("Jagged Little Pill", "Alanis Morissette", 1995, 2780000),
    ("Tubular Bells", "Mike Oldfield", 1973, 2760000),
    ("Scissor Sisters", "Scissor Sisters", 2004, 2760000),
    ("...But Seriously", "Phil Collins", 1989, 2750000),
    ("Tracy Chapman", "Tracy Chapman", 1988, 2710000),
    ("Parachutes", "Coldplay", 2000, 2710000),
    ("The Man Who", "Travis", 1999, 2687500),
    ("Greatest Hits", "ABBA", 1975, 2606000),
    ("I've Been Expecting You", "Robbie Williams", 1998, 2586500),
    ("Come Away with Me", "Norah Jones", 2002, 2556650),
    ("Graceland", "Paul Simon", 1986, 2500000),
    ("Ladies & Gentlemen: The Best of", "George Michael", 1998, 2500000) ]


-- 
--
--  Your functional code goes here
--
--

albumsToString :: [Album] -> String
albumsToString [] = ""
albumsToString ((ti,ar,yr,sa):xs) =  "\nTitle: " ++ formatText ti ++ "Artist: " ++ formatText ar ++ "Year " ++ show(yr) ++ "     Sales: " ++ show(sa) ++ "\n" ++ albumsToString xs


top10 :: [Album] -> [Album]
top10 x = take 10 x

albumsReleasedBetween :: Int -> Int -> [Album] -> [Album]
albumsReleasedBetween st end li = [(ti, ar, yr, sa) | (ti, ar, yr, sa) <- li, yr >= st && yr <= end]

albumsWithPrefix :: String -> [Album] -> [Album]
albumsWithPrefix pr li = [(ti, ar, yr, sa) | (ti, ar, yr, sa) <- li, isPrefixOf pr ti ]

albumsOfArtist :: String -> [Album] -> [Album]
albumsOfArtist name li = [(ti, ar, yr, sa) | (ti, ar, yr, sa) <- li, name == ar] 

totalSalesOfArtist :: [Album] -> Int
totalSalesOfArtist [] = 0
totalSalesOfArtist ((ti, ar, yr, sa):xs) = sa + totalSalesOfArtist xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

artistTuple :: Album -> (String, Int)
artistTuple (ti, ar, yr, sa) = (ar, (listLength(albumsOfArtist ar testData)))

albumsInChart :: [Album] -> [(String, Int)]
albumsInChart xs = map (artistTuple) xs

tupleToString :: [(String, Int)] -> String
tupleToString [] = " "
tupleToString ((st, int):xs) =  formatText st ++  show(int) ++ "\n" ++ tupleToString xs

fourth :: Album -> Int
fourth (_,_,_,a) = a

reverseList :: [Album] -> [Album]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

sortData :: [Album] -> [Album]
sortData x = reverseList(sortOn fourth x)

deleteFiftieth :: [Album] -> [Album]
deleteFiftieth x = take 49 x

addNewAlbum :: Album -> [Album] -> [Album]
addNewAlbum al li = sortData (li ++ [al])

first :: Album -> String
first (a,_,_,_) = a

secnd :: Album -> String
secnd (_,a,_,_) = a

third :: Album -> Int
third (_,_,a,_) = a


selectFst :: [Album] -> Album
selectFst (x:xs) = x

increaseSales :: Title -> Artist -> Int -> [Album] -> [Album]
increaseSales title artist salesInc li
    | length li > 0 = sortData ([increase (head li) title artist salesInc] ++ increaseSales title artist salesInc (tail li))
    | otherwise = []
    
increase :: Album -> Title -> Artist -> Int -> Album
increase (ti, ar, yr, sa) title artist salesInc
    | ti == title && ar == artist = (ti, ar, yr, sa + salesInc)
    | otherwise = (ti, ar, yr, sa)

formatText :: String -> String
formatText str
    | length str <= 45 = formatText (str ++ " ")
    | otherwise = str

-- Demo function to test basic functionality (without persistence - i.e. 
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (albumsToString (albumsReleasedBetween 2000 2008 testData))
demo 4  = putStrLn (albumsToString (albumsWithPrefix "Th" testData))
demo 5  = putStrLn (show(totalSalesOfArtist(albumsOfArtist "Queen" testData)))
demo 6  = putStrLn (tupleToString (albumsInChart testData ))
demo 7  = putStrLn (albumsToString (sortData(addNewAlbum("Progress", "Take That", 2010, 2700000)(deleteFiftieth testData))))
demo 8  = putStrLn (albumsToString (increaseSales "21" "Adele" 400000 testData)) 


--
--
-- Your user interface (and loading/saving) code goes here
--
--

main :: IO ()
main = do
    contents <- readFile "Albums.txt"
    putStr "\n\nMAIN MENU\nPlease select an option by entering the corresponding number:\n1)View Top 50\n2)View the top ten by sales\n3)See all albums in the top 50 that were released between two years\n4)Search for albums by prefix\n5)View the total sales figures for an artist\n6)View how many albums each artist has in the top 50\n7)Add a new album to the top 50 (and remove the 50th)\n8)Increase the sales of an album\n\n"
    opt <- getLine
    if (opt == "") then do
        return ()
    else
        if (opt == "1") then do            
            let li = (toList contents) in
                putStr (wordsToString li)
            main
        else
            if (opt == "2") then do                
                let li = (top10 (toList contents)) in
                    putStr (wordsToString li)
                main 
            else
                if(opt == "3") then do                    
                    putStr "Enter first year: "
                    frYr <- getLine
                    putStr "Enter second year: "
                    scndYr <- getLine
                    let li = (albumsReleasedBetween (read frYr :: Int) (read scndYr :: Int) (toList contents)) in
                        putStr (wordsToString li)
                    main             
                else                   
                    if (opt == "4") then do                        
                        putStr "Please enter the prefix you wish to search for: "
                        prefix <- getLine
                        let li = (albumsWithPrefix prefix (toList contents)) in
                            putStr (wordsToString li)
                        main
                    else
                        if (opt == "5") then do                            
                            putStr "Please enter the name of the artist: "
                            artist <- getLine
                            let li = (totalSalesOfArtist(albumsOfArtist artist (toList contents))) in
                                putStr (show li ++ "\n")
                            main
                        else
                            if (opt == "6") then do                                
                                putStr (tupleToString (albumsInChart (toList contents) ))
                                main
                            else
                                if (opt == "7") then do                                    
                                    putStr "Please Enter Title: "
                                    title <- getLine
                                    putStr "Please Enter Artist: "
                                    artist <- getLine
                                    putStr "Please Enter Year: "
                                    year <- getLine
                                    putStr "Please Enter Sales: "
                                    sales <- getLine
                                    let li = (addNewAlbum(title, artist, (read year :: Int), (read sales :: Int))(deleteFiftieth (toList contents))) in do
                                        putStr ((wordsToString li) ++ "\nSave Changes:\n1)Yes\n2)No\n")
                                        sveCh <- getLine
                                        if (sveCh == "1") then do
                                            putStr "\n\nPlease enter the name you wish to give you're save: "
                                            saveName <- getLine
                                            writeFile (saveName ++ ".txt") (show li)
                                            main
                                        else
                                            main                                                                              
                                    main
                                else
                                    if (opt == "8") then do                                         
                                        putStr "Please enter Title: "
                                        title <- getLine
                                        putStr "Please enter Artist: "
                                        artist <- getLine
                                        putStr "Please enter the ammount of additional sales: "
                                        salesInc <- getLine
                                        let li = (increaseSales title artist (read salesInc :: Int) (toList contents)) in do
                                            putStr ((wordsToString li) ++ "\nSave Changes:\n1)Yes\n2)No\n")
                                            sveCh <- getLine
                                            if (sveCh == "1") then do
                                                putStr "\n\nPlease enter the name you wish to give you're save: "
                                                saveName <- getLine
                                                writeFile (saveName ++ ".txt") (show li)
                                                main
                                            else
                                                main
                                        main
                                      
                                    else do
                                        putStr "\nPlease enter a valid option\n"
                                        main

wordsToString :: [(String, String, Int, Int)] -> String
wordsToString [] = " "
wordsToString ((ti, ar, yr, sa):xs) = "\n" ++ formatText ti ++ " "++ formatText ar ++ " " ++ show yr ++ "    " ++ show sa ++ "\n" ++ wordsToString xs

toList :: String -> [Album]
toList = read

