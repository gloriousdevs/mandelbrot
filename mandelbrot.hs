import Data.Complex
import Data.List

type Samples = [Complex Float]


absolute_threshold = 2
steps_threshold = 42

width, height, sample_size :: Float
width  = 20000.0
height = 20000.0
sample_size = width * height

min_x = -2.0
max_x =  2.0
min_y = -2.0
max_y =  2.0
  
mandelbrot_map :: Complex Float -> Int
mandelbrot_map c = mandelbrot_map' (0 :+ 0) c 0

mandelbrot_map' :: Complex Float -> Complex Float -> Int -> Int
mandelbrot_map' z c s
    | s == steps_threshold               = s
    | (magnitude z) >= absolute_threshold  = s
    | otherwise                          = mandelbrot_map' (new_z z c) c (s + 1)
    where new_z z c = (z ^ 2) + c

generate_samples :: [Samples]
generate_samples = [[get_coordinate l c | c <- [1.0..width]] | l <- [1.0..height] :: [Float]]

get_coordinate :: Float -> Float -> Complex Float
get_coordinate l c = (x :+ y)
  where x = min_x + (x_step * c)
        y = max_y - (y_step * l)
        x_step = (max_x - min_x) / width
        y_step = (max_y - min_y) / height

plot_it :: Int -> String
plot_it s
  | s == steps_threshold = "0"
  | otherwise = "1"

map_steps :: [[Int]]
map_steps = map (\s -> map mandelbrot_map s) generate_samples

map_pixels :: [String]
map_pixels = map (\s -> concat $ map plot_it s) map_steps

generate_data = unlines map_pixels

dimension = show (round width)  ++ " " ++ show (round height)


main = do
  putStrLn "Hello! Writing Mandelbrot set..."

  writeFile "./foo.bmp" "P1\n" 
  appendFile "./foo.bmp" (dimension ++ "\n")
  appendFile "./foo.bmp" generate_data
  

  return ()
  -- writeFile "./foo.bmp" "" 

-- chunksOf :: Int -> Samples -> [Samples]
-- chunksOf _ [] = []
-- chunksOf n xs = chunk : chunksOf n rest
--   where chunk = take n xs
--         rest = drop n xs
--
-- generate_chunks :: Samples -> [Samples]
-- generate_chunks xs = chunksOf width xs

{-
  P1
  # This is an example bitmap of the letter "J"
  6 10
-}
