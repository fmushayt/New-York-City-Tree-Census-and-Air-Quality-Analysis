# match zipcodes to respective UHF42
UH101 = as.character(c(10463,10471))
UH102 = as.character(c(10464,10466,10469,10470,10475))
UH103 = as.character(c(10458,10467,10468))
UH104 = as.character(c(10461,10462,10465,10472,10473))
UH105 = as.character(c(10453,10457,10460))
UH106 = as.character(c(10451,10452,10456))
UH107 = as.character(c(10454,10455,10459,10474))
UH201 = as.character(c(11211,11222))
UH202 = as.character(c(11201,11205,11215,11217,11231))
UH203 = as.character(c(11212,11213,11216,11233,11238))
UH204 = as.character(c(11207,11208))
UH205 = as.character(c(11220,11232))
UH206 = as.character(c(11204,11218,11219,11230))
UH207 = as.character(c(11203,11210,11225,11226))
UH208 = as.character(c(11234,11236,11239))
UH209 = as.character(c(11209,11214,11228))
UH210 = as.character(c(11223,11224,11229,11235))
UH211 = as.character(c(11206,11221,11237))
UH301 = as.character(c(10031,10032,10033,10034,10040))
UH302 = as.character(c(10026,10027,10030,10037,10039))
UH303 = as.character(c(10029,10035))
UH304 = as.character(c(10023,10024,10025))
UH305 = as.character(c(10021,10028,10044,10128))
UH306 = as.character(c(10001,10011,10018,10019,10020,10036))
UH307 = as.character(c(10010,10016,10017,10022))
UH308 = as.character(c(10012,10013,10014))
UH309 = as.character(c(10002,10003,10009))
UH310 = as.character(c(10004,10005,10006,10007,10038,10280))
UH401 = as.character(c(11101,11102,11103,11104,11105,11106))
UH402 = as.character(c(11368,11369,11370,11372,11373,11377,11378))
UH403 = as.character(c(11354,11355,11356,11357,11358,11359,11360))
UH404 = as.character(c(11361,11362,11363,11364))
UH405 = as.character(c(11374,11375,11379,11385))
UH406 = as.character(c(11365,11366,11367))
UH407 = as.character(c(11414,11415,11416,11417,11418,11419,11420,11421))
UH408 = as.character(c(11412,11423,11430,11432,11433,11434,11435,11436,11001))
UH409 = as.character(c(11004,11005,11411,11413,11422,11426,11427,11428,11429))
UH410 = as.character(c(11691,11692,11693,11694,11695,11697))
UH501 = as.character(c(10302,10303,10310))
UH502 = as.character(c(10301,10304,10305))
UH503 = as.character(c(10314))
UH504 = as.character(c(10306,10307,10308,10309,10312))

# function to convert zipcode to UHF42
convertUHF <- function(zipcode) {
  j = 1
  UHF = vector(mode = "character", length(zipcode))
  for (i in zipcode) {
    if (i %in% UH101) {
      UHF[j] = "101"
      j = j + 1
    }
    else if (i %in% UH102) {
      UHF[j] = "102"
      j = j + 1
    }
    else if (i %in% UH103) {
      UHF[j] = "103"
      j = j + 1
    }
    else if (i %in% UH104) {
      UHF[j] = "104"
      j = j + 1
    }
    else if (i %in% UH105) {
      UHF[j] = "105"
      j = j + 1
    }
    else if (i %in% UH106) {
      UHF[j] = "106"
      j = j + 1
    }
    else if (i %in% UH107) {
      UHF[j] = "107"
      j = j + 1
    }
    else if (i %in% UH201) {
      UHF[j] = "201"
      j = j + 1
    }
    else if (i %in% UH202) {
      UHF[j] = "202"
      j = j + 1
    }
    else if (i %in% UH203) {
      UHF[j] = "203"
      j = j + 1
    }
    else if (i %in% UH204) {
      UHF[j] = "204"
      j = j + 1
    }
    else if (i %in% UH205) {
      UHF[j] = "205"
      j = j + 1
    }
    else if (i %in% UH206) {
      UHF[j] = "206"
      j = j + 1
    }
    else if (i %in% UH207) {
      UHF[j] = "207"
      j = j + 1
    }
    else if (i %in% UH208) {
      UHF[j] = "208"
      j = j + 1
    }
    else if (i %in% UH209) {
      UHF[j] = "209"
      j = j + 1
    }
    else if (i %in% UH210) {
      UHF[j] = "210"
      j = j + 1
    }
    else if (i %in% UH211) {
      UHF[j] = "211"
      j = j + 1
    }
    else if (i %in% UH301) {
      UHF[j] = "301"
      j = j + 1
    }
    else if (i %in% UH302) {
      UHF[j] = "302"
      j = j + 1
    }
    else if (i %in% UH303) {
      UHF[j] = "303"
      j = j + 1
    }
    else if (i %in% UH304) {
      UHF[j] = "304"
      j = j + 1
    }
    else if (i %in% UH305) {
      UHF[j] = "305"
      j = j + 1
    }
    else if (i %in% UH306) {
      UHF[j] = "306"
      j = j + 1
    }
    else if (i %in% UH307) {
      UHF[j] = "307"
      j = j + 1
    }
    else if (i %in% UH308) {
      UHF[j] = "308"
      j = j + 1
    }
    else if (i %in% UH309) {
      UHF[j] = "309"
      j = j + 1
    }
    else if (i %in% UH310) {
      UHF[j] = "310"
      j = j + 1
    }
    else if (i %in% UH401) {
      UHF[j] = "401"
      j = j + 1
    }
    else if (i %in% UH402) {
      UHF[j] = "402"
      j = j + 1
    }
    else if (i %in% UH403) {
      UHF[j] = "403"
      j = j + 1
    }
    else if (i %in% UH404) {
      UHF[j] = "404"
      j = j + 1
    }
    else if (i %in% UH405) {
      UHF[j] = "405"
      j = j + 1
    }
    else if (i %in% UH406) {
      UHF[j] = "406"
      j = j + 1
    }
    else if (i %in% UH407) {
      UHF[j] = "407"
      j = j + 1
    }
    else if (i %in% UH408) {
      UHF[j] = "408"
      j = j + 1
    }
    else if (i %in% UH409) {
      UHF[j] = "409"
      j = j + 1
    }
    else if (i %in% UH410) {
      UHF[j] = "410"
      j = j + 1
    }
    else if (i %in% UH501) {
      UHF[j] = "501"
      j = j + 1
    }
    else if (i %in% UH502) {
      UHF[j] = "502"
      j = j + 1
    }
    else if (i %in% UH503) {
      UHF[j] = "503"
      j = j + 1
    }
    else if (i %in% UH504) {
      UHF[j] = "504"
      j = j + 1
    }
  }
  return (UHF)
}