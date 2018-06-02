-- Use of  a custom type  Texteditor
data Texteditor = Texteditor ([Char],[Char],[Char],[Char])
     deriving Show
t :: Texteditor
t = Texteditor ( "The cat sat", "Highlight", "on the mat", "Buffer")
-- l = left of the cursor 
-- h = highlighted text 
-- q = right of the cursor 
-- b = buffer text used in cut ,copy and paste
-- init function which resets the texteditor to a default state in which all lists are empty
initalize:: Texteditor -> Texteditor
initalize (Texteditor (l,h,q,b)) =  (Texteditor([],[],[],[]))

--cursorRight function that moves the cursor right by one character
cursorRight:: Texteditor-> Texteditor
cursorRight (Texteditor (l ,h ,q,b))
-- if the list on the right of the cursor is empty then do this line below 
 | q == [] = (Texteditor (l, h, [], b))
-- if the list on the right of the cursor is not empty then do this line below
 |otherwise = (Texteditor (l ++ h ++ [head(q)], [], tail(q), b))

--cursorLeft function that moves the cursor left by one character
cursorLeft:: Texteditor -> Texteditor
cursorLeft (Texteditor (l ,h ,q,b))
-- if the list on the left of the cursor is empty then do this line below 
 | l == [] = (Texteditor([],h,q,b))
-- if the list on the left of the cursor is not empty then do this line below
 |otherwise = (Texteditor(head(l):reverse(tail(reverse(tail(l)))),[],head(reverse(tail(l))): h++q ,b ))

--cursorFarRight function moves the cursor to far right
cursorFarRight:: Texteditor -> Texteditor
cursorFarRight(Texteditor (l ,h ,q,b))
-- if the list on the right of the cursor is empty then do this line below 
 | q == [] = (Texteditor (l, h, [], b))
-- if the list on the right of the cursor is not empty then do this function below
 | otherwise = (Texteditor(l++h++q,[],[],b))

--cursorFarLeft function moves the cursor to far left
cursorFarLeft:: Texteditor -> Texteditor
cursorFarLeft(Texteditor (l ,h ,q,b))
-- if the list on the left of the cursor is empty then do this line below 
 | l == [] = (Texteditor([],h,q,b))
-- if the list on the left of the cursor is not empty then do this function below
 | otherwise  = (Texteditor([],[], l++h++q,b))

--cursorRightWord function moves cursor to the next word on the right of the cursor
cursorRightWord::Texteditor ->Texteditor
cursorRightWord(Texteditor (l ,h ,q,b))
-- if the list on the right of the cursor is empty then do this line below 
 | q == [] = (Texteditor (l, h, [], b))
--cursorRightWord recursively searchs the list on the right of the cursor until it reaches a space
 | head(q) == ' ' = (Texteditor(l,h,q,b))
--if the head of the right list is not a space call the cursorRightWord function again
 | otherwise = cursorRightWord(Texteditor(l++h++[head(q)],[],tail(q),b))

--cursorLeftWord function moves cursor to the next word on the left of the cursor	
cursorLeftWord:: Texteditor -> Texteditor
cursorLeftWord(Texteditor (l ,h ,q,b))
-- if the list on the left of the cursor is empty then do this line below 
 | l == [] = (Texteditor([],h,q,b))
--cursorLeftWord recursively searchs the list on the left of the cursor until it reaches a space
 | head(reverse(l)) == ' ' = (Texteditor(head(l):reverse(tail(reverse(tail(l)))),[],[head(reverse(tail(l)))]++h++q,b))
--if the last item in the left list is not a space call the cursorLeftWord function again
 | otherwise =  cursorLeftWord(Texteditor(head(l):reverse(tail(reverse(tail(l)))),[],[head(reverse(tail(l)))]++h++q,b))

--write function
write:: Texteditor -> Char -> Texteditor
write(Texteditor (l ,h ,q,b)) c 
-- write function adds a character to the left of the cursor only if the texteditor has less than 1024 characters in all the lists
 | length (l++[c]++h++q)<1024 =(Texteditor(l++[c],[],q,b))
-- if it has 1024 it doesn't add a character
 |otherwise = (Texteditor(l,h,q,b))

-- backspace function
backspace:: Texteditor -> Texteditor
backspace (Texteditor (l ,h ,q,b))
-- if the list on the left of the cursor is empty then do this line below 
 | l == [] = (Texteditor([],h,q,b))
-- remove the last character on the left of the cursor list
 |otherwise = (Texteditor(head(l):reverse(tail(reverse(tail(l)))),[],q,b))

--delete function
delete:: Texteditor -> Texteditor 
delete (Texteditor (l ,h ,q,b))
-- if the list on the right of the cursor is empty then do this line below 
 | q == [] = (Texteditor (l, h, [], b))
-- remove the head of the right of the cursor list to delete
 | otherwise = (Texteditor(l,[], tail(q),b))

-- highlightLeftCharacter function highlights the character to the left of the cursor
highlightLeftCharacter:: Texteditor -> Texteditor
highlightLeftCharacter(Texteditor (l ,h ,q,b))
-- if the list on the left of the cursor is empty then do this line below 
 | l == [] = (Texteditor([],h,q,b))
-- highlight the character to the left of the cursor
 | otherwise = (Texteditor(head(l):reverse(tail(reverse(tail(l)))),[head(reverse(tail(l)))]++h,q,b ))

--highlightRightCharacter function highlights the character to the right of the cursor 
highlightRightCharacter:: Texteditor-> Texteditor
highlightRightCharacter(Texteditor (l ,h ,q,b))
-- if the list on the right of the cursor is empty then do this line below 
 | q == [] = (Texteditor (l, h, [], b))
-- highlight the character to the right of the cursor
 | otherwise = (Texteditor(l,h++[head(q)],tail(q),b))

--highlightRightWord function highlights the word to the right of the cursor
highlightRightWord :: Texteditor -> Texteditor
highlightRightWord (Texteditor (l,h,q,b))
 -- if the list on the right of the cursor is empty then do this 
 | q == [] = (Texteditor(l,h,q,b))
 --highlightRightWord recursively searchs the list on the right of the cursor until it reaches a space
 | head(q) == ' ' = (Texteditor(l,h++[head(q)],tail(q),b))
 --if the head of the right list is not a space call the highlightRightWord function again
 | otherwise =  highlightRightWord(Texteditor(l,h++[head(q)],tail(q),b))

 
 
 
--highlightLeftWord function highlights the word to the left  of the cursor 
highlightLeftWord :: Texteditor -> Texteditor
highlightLeftWord (Texteditor(l,h,q,b))
-- if the list on the left of the cursor is not empty then do this function below
 | l == [] = (Texteditor(l,h,q,b))
--highlightLeftWord recursively searchs the list on the left of the cursor until it reaches a space
 | head(reverse(tail(l))) == ' ' = (Texteditor (head(l):reverse(tail(reverse(tail(l)))),[head(reverse(tail(l)))]++h,q,b))
    --if the last item in the left list is not a space call the highlightLeftWord function again
 | otherwise = highlightLeftWord(Texteditor(head(l):reverse(tail(reverse(tail(l)))),[head(reverse(tail(l)))]++h,q,b))

--highlightAllLeft function highlights everything on the left of the cursor
highlightAllLeft :: Texteditor -> Texteditor
highlightAllLeft (Texteditor(l,h,q,b)) = (Texteditor([],l++h,q,b))

--highlightAllRight function highlights everything on the right of the cursor 
highlightAllRight :: Texteditor -> Texteditor
highlightAllRight (Texteditor(l,h,q,b)) = (Texteditor(l,h++q,[],b))

--highlightWholeLine highlights the whole line 	
highlightWholeLine :: Texteditor -> Texteditor
highlightWholeLine (Texteditor(l,h,q,b)) = (Texteditor([],l++ h ++q,[],b))

--cut function cuts the highlight and  puts it in buffer
cut:: Texteditor -> Texteditor
cut (Texteditor(l,h,q,b)) = (Texteditor(l,[],q,h))

--copy function copys the highlighted text and puts it in buffer
copy:: Texteditor -> Texteditor
copy (Texteditor(l,h,q,b)) =(Texteditor(l,h,q,h))

-- paste function  pastes the text from the buffer to the left of the cursor
paste:: Texteditor -> Texteditor
paste (Texteditor(l,h,q,b))
-- if it is not greater than 1024 characters it adds the buffer to the left of the cursor list
 | length (l++q++b)<1024 = (Texteditor(l++b,[],q,b))
-- if it greater than 1024 characters it doesn't add the buffer to the left of the cursor list
 | otherwise = (Texteditor(l,h,q,b))
