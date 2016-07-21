# diff-in-ML
An implementation of `diff` in ML using the Longest Common Subsequence algorithm

- This program compares two strings and returns its result of the comparison

- The result of a comparison is actually a list of hunks (in the form of pairs of `int`s), which can be one of:  
  `(0, n)` denoting n characters added (in the right only)  
  `(m, 0)` denoting m characters deleted (in the left only)  
  `(m, n)` denoting m characters are changed into n characters  
  `(-1, n)` denoting the same n characters  

- For example,  
`test "xy" "abc"` gives `[2,3]` denoting 2 characters from the left turns into 3 characters in the right  
`test "abc" "abc"` gives `[-1,3]` denoting 3 characters are common in both sides  
`test "abc" "abd"` gives `[-1,2; 1,1]` denoting the first 2 characters are same, but then the next 1 character from the left  turns into the next 1 character in the right  
`test "abc" "abde"` gives `[-1,2; 1,2]` denoting the first 2 characters are same, but then the next 1 character from the left turns into the next 2 characters in the right  
`test "abd" "abcd"` gives `[-1,2; 0,1; -1,1]` denoting the first 2 characters are same, then the 1 character is added to the right, and then the next 1 character comes same in both sides  
`test "abcd" "abd"` gives `[-1,2; 1,0; -1,1]` denoting the first 2 characters are same, then the 1 character is deleted from the left, and then the next 1 character comes same in both sides  
`test "abcehjlmnp" "bcdefjklmrst"` gives `[1,0; -1,2; 0,1; -1,1; 1,1; -1,1; 0,1; -1,2; 2,3]`  
`test "" "abc"` gives `[]` denoting an empty string was reached in either side  
