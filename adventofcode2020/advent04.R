input <- readLines("~/workspace-private/adventofcode2020/input04.txt")

require(stringi)
input <-"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"
getPassports <- function(input){
  a <- stri_extract_all_regex(stri_join(input, collapse = "\n"), "(\\w+:[#\\w\\d]+[ \n]?)+(\n\n|$)")[[1]]
  stri_count_regex(a, "\\w+:")
  
  b <- stri_extract_all_regex(a, "(\\w+:[#\\w\\d]+)")
  b
}


isValidPart1<- function(x){
  length(x) == 8 || (length(x) == 7 && all(stri_sub(x,1,3) != "cid"))
}
# part 1
sum(sapply(getPassports(input), isValidPart1))

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
isValidPart2<- function(values){
  for(x in values){
    key <- stri_sub(x, 1,3)
    value <- stri_sub(x, 5)
    if(key == "byr"){
      if(!inRange(value, 1920, 2002)){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
    } else if(key == "iyr"){
      if(!inRange(value, 2010, 2020)){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
    } else if(key == "eyr"){
      if(!inRange(value, 2020, 2030)){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
    } else if(key == "hgt"){
      if(!stri_detect_regex(value, "cm|in$")){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
      type <- stri_sub(value, -2)
      hgt <- stri_sub(value, 1, -3)
      if(type == "in"){
        if(!inRange(hgt, 59, 76)){
          cat(x, " is invalid","\n")
          return(FALSE)
        }
      }else{
        if(!inRange(hgt, 150, 193)){
          cat(x, " is invalid","\n")
          return(FALSE)
        }
      }
    } else if(key == "hcl"){
      if(!stri_detect_regex(value, "^#[0-9a-f]{6}$")){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
    } else if(key == "ecl"){
      if(!stri_detect_regex(value, "^amb|blu|brn|gry|grn|hzl|oth$")){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
    }else if(key == "pid"){
      if(!stri_detect_regex(value, "^[0-9]{9}$")){
        cat(x, " is invalid","\n")
        return(FALSE)
      }
    }
  }
  return(isValidPart1(values))
}
# part 2
sum(sapply(getPassports(input), isValidPart2))

inRange <- function(x, min, max){
  int <- as.integer(x)
  return(int >= min && int <=max)
}

isValidPart2(values)
values

invalid <- "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"

valid <- "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

sum(sapply(getPassports(invalid), isValidPart2))
sum(sapply(getPassports(valid), isValidPart2))

