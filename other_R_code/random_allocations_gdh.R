# Random allocation test
test_coding_group <- c(7, 9)
test_tranche_docs <- c(266:268)
allocate_randomly(user_ids = test_coding_group,
                  set = test_tranche_docs, coder_rate = 1,
                  allocation_type = "coding", allocated_by = "test_for_deletion",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation test 2
test_coding_group <- c(7, 9)
test_tranche_docs <- c(260:262)
allocate_randomly(user_ids = test_coding_group,
                  set = test_tranche_docs, coder_rate = 1,
                  allocation_type = "coding", allocated_by = "test_for_deletion",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 1
first_coding_group <- c(9, 11)
first_tranche_docs <- c(208:217)
allocate_randomly (user_ids = first_coding_group,
                   set = first_tranche_docs, coder_rate = 2,
                   allocation_type = "coding", allocated_by = "regular_random_assignment",
                   restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 2
second_coding_group <- c(8:150)
second_tranche_docs <- c(218:268)
allocate_randomly(user_ids = second_coding_group,
                  set = second_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 3
third_coding_group <- c(8, 9, 11, 12, 15, 16)
third_tranche_docs <- c(269:361)
allocate_randomly(user_ids = third_coding_group,
                  set = third_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 4
fourth_coding_group <- c(8, 21, 22, 38)
fourth_tranche_docs <- c(362:420)
allocate_randomly(user_ids = fourth_coding_group,
                  set = fourth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 5
fifth_coding_group <- c(19, 22, 24, 28, 31, 38, 45)
fifth_tranche_docs <- c(421:476)
allocate_randomly(user_ids = fifth_coding_group,
                  set = fifth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 6
sixth_coding_group <- c(8, 11, 21, 22, 29, 38, 40, 45, 47)
sixth_tranche_docs <- c(477:547)
allocate_randomly(user_ids = sixth_coding_group,
                  set = sixth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 7
seventh_coding_group <- c(19, 23, 28, 36, 37, 39, 43, 45)
seventh_tranche_docs <- c(548:648)
allocate_randomly(user_ids = seventh_coding_group,
                  set = seventh_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 8
eighth_coding_group <- c(18, 25, 27, 41, 31, 8, 38)
eighth_tranche_docs <- c(649:708)
allocate_randomly(user_ids = eighth_coding_group,
                  set = eighth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 9
ninth_coding_group <- c(9, 11, 16, 21, 22, 24, 29, 31, 40, 41)
ninth_tranche_docs <- c(709:800)
allocate_randomly(user_ids = ninth_coding_group,
                  set = ninth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 10
tenth_coding_group <- c(19, 38)
tenth_tranche_docs <- c(801:827)
allocate_randomly(user_ids = tenth_coding_group,
                  set = tenth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 11
eleventh_coding_group <- c(8, 12, 16, 21, 22, 23, 27, 28, 29, 31, 39, 40)
eleventh_tranche_docs <- c(828:888)
allocate_randomly(user_ids = eleventh_coding_group,
                  set = eleventh_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 12
twelfth_coding_group <- c(17, 46, 38)
twelfth_tranche_docs <- c(889:916)
allocate_randomly(user_ids = twelfth_coding_group,
                  set = twelfth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 13
thirteenth_coding_group <- c(8, 9, 21, 25, 31, 36, 41)
thirteenth_tranche_docs <- c(917:954)
allocate_randomly(user_ids = thirteenth_coding_group,
                  set = thirteenth_tranche_docs, coder_rate = 2,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)


# NEW FUNCTION Random allocation 14
fourteenth_double_coding_group <- c(26, 34, 42, 44)
fourteenth_single_coding_group <- c(8, 19, 29, 38)
fourteenth_tranche_docs <- c(955:1047)
allocate_randomly_with_some_double_assignment(double_coding_user_ids = fourteenth_double_coding_group,
                                              other_user_ids = fourteenth_single_coding_group,
                                              set = fourteenth_tranche_docs,
                                              n_double_coding = 20,
                                              coder_rate = 1.1,
                                              allocation_type = "coding",
                                              allocated_by = "irregular_random_assignment",
                                              restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 15
fifteenth_double_coding_group <- c(35)
fifteenth_single_coding_group <- c(22, 24, 25, 27, 31, 36, 39, 41)
fifteenth_tranche_docs <- c(1048:1090)
allocate_randomly_with_some_double_assignment(double_coding_user_ids = fifteenth_double_coding_group,
                                              other_user_ids = fifteenth_single_coding_group,
                                              set = fifteenth_tranche_docs,
                                              n_double_coding = 20,
                                              coder_rate = 1.1,
                                              allocation_type = "coding",
                                              allocated_by = "irregular_random_assignment",
                                              restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 16
sixteenth_coding_group <- c(12, 21, 22, 24, 27, 36, 38, 39, 41, 47)
sixteenth_tranche_docs <- c(1091:1134)
allocate_randomly(user_ids = sixteenth_coding_group,
                  set = sixteenth_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 17
seventeenth_coding_group <- c(40, 16, 21, 27, 38, 41)
seventeenth_tranche_docs <- c(1135:1200)
allocate_randomly(user_ids = seventeenth_coding_group,
                  set = seventeenth_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 18
eighteenth_coding_group <- c(12, 19, 24, 25, 28, 31, 39, 45)
eighteenth_tranche_docs <- c(1201:1245)
allocate_randomly(user_ids = eighteenth_coding_group,
                  set = eighteenth_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 19
nineteenth_coding_group <- c(8, 9, 11, 12, 22, 36)
nineteenth_tranche_docs <- c(1246:1268)
allocate_randomly(user_ids = nineteenth_coding_group,
                  set = nineteenth_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 20
twenty_coding_group <- c(8, 38)
twenty_tranche_docs <- c(1269:1300)
allocate_randomly(user_ids = twenty_coding_group,
                  set = twenty_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 21
twentyone_double_coding_group <- c(10)
twentyone_single_coding_group <- c(8, 21, 24, 25, 31)
twentyone_tranche_docs <- c(1301:1351)
allocate_randomly_with_some_double_assignment(double_coding_user_ids = twentyone_double_coding_group,
                                              other_user_ids = twentyone_single_coding_group,
                                              set = twentyone_tranche_docs,
                                              n_double_coding = 20,
                                              coder_rate = 1.1,
                                              allocation_type = "coding",
                                              allocated_by = "irregular_random_assignment",
                                              restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 22
twentytwo_coding_group <- c(8, 19, 22, 31, 38, 42)
twentytwo_tranche_docs <- c(1352:1425)
allocate_randomly(user_ids = twentytwo_coding_group,
                  set = twentytwo_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 23
twentythree_coding_group <- c(9, 12, 21, 41)
twentythree_tranche_docs <- c(1426:1476)
allocate_randomly(user_ids = twentythree_coding_group,
                  set = twentythree_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 24
twentyfour_double_coding_group <- c(14)
twentyfour_single_coding_group <- c(8, 9, 11, 19, 22, 23, 24, 25, 29, 34, 35, 38, 39, 41, 43, 44, 47)
twentyfour_tranche_docs <- c(1477:1562)
allocate_randomly_with_some_double_assignment(double_coding_user_ids = twentyfour_double_coding_group,
                                              other_user_ids = twentyfour_single_coding_group,
                                              set = twentyfour_tranche_docs,
                                              n_double_coding = 10,
                                              coder_rate = 1.1,
                                              allocation_type = "coding",
                                              allocated_by = "irregular_random_assignment",
                                              restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 25
twentyfive_coding_group <- c(10, 19, 29, 34, 40)
twentyfive_tranche_docs <- c(1563:1613)
allocate_randomly(user_ids = twentyfive_coding_group,
                  set = twentyfive_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 26
twentysix_coding_group <- c(10, 24, 25, 31)
twentysix_tranche_docs <- c(1614:1654)
allocate_randomly(user_ids = twentysix_coding_group,
                  set = twentysix_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 27
twentyseven_coding_group <- c(8, 21, 22, 23, 31, 36, 39, 43)
twentyseven_tranche_docs <- c(1655:1684)
allocate_randomly(user_ids = twentyseven_coding_group,
                  set = twentyseven_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 28
twentyeight_coding_group <- c(8, 40)
twentyeight_tranche_docs <- c(1685:1710)
allocate_randomly(user_ids = twentyeight_coding_group,
                  set = twentyeight_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 29
twentynine_coding_group <- c(22, 23, 39, 36, 39, 40, 43)
twentynine_tranche_docs <- c(1711:1731)
allocate_randomly(user_ids = twentynine_coding_group,
                  set = twentynine_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 30
thirty_coding_group <- c(29, 31, 40)
thirty_tranche_docs <- c(1732:1770)
allocate_randomly(user_ids = thirty_coding_group,
                  set = thirty_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 31
thirtyone_coding_group <- c(22, 38)
thirtyone_tranche_docs <- c(1771:1794)
allocate_randomly(user_ids = thirtyone_coding_group,
                  set = thirtyone_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 32
thirtytwo_coding_group <- c(31, 39, 40, 42)
thirtytwo_tranche_docs <- c(1795:1835)
allocate_randomly(user_ids = thirtytwo_coding_group,
                  set = thirtytwo_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 33
thirtythree_coding_group <- c(10, 19, 24, 34, 38)
thirtythree_tranche_docs <- c(1836:1896)
allocate_randomly(user_ids = thirtythree_coding_group,
                  set = thirtythree_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 34
thirtyfour_coding_group <- c(8, 9, 11, 12, 23, 29, 35, 39, 40, 41)
thirtyfour_tranche_docs <- c(1897:1997)
allocate_randomly(user_ids = thirtyfour_coding_group,
                  set = thirtyfour_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 35
thirtyfive_coding_group <- c(21, 25, 34, 36, 38, 44)
thirtyfive_tranche_docs <- c(1998:2009)
allocate_randomly(user_ids = thirtyfive_coding_group,
                  set = thirtyfive_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 36
thirtysix_coding_group <- c(21, 27, 29, 34, 36, 44, 47)
thirtysix_tranche_docs <- c(2010:2030)
allocate_randomly(user_ids = thirtysix_coding_group,
                  set = thirtysix_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 37
thirtyseven_coding_group <- c(8, 9, 10, 22, 25, 38, 39, 43)
thirtyseven_tranche_docs <- c(2031:2112)
allocate_randomly(user_ids = thirtyseven_coding_group,
                  set = thirtyseven_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 38
thirtyeight_coding_group <- c(8, 9, 10, 11, 12, 19, 21, 22, 23, 24, 27, 28, 29, 31, 34, 35, 36, 38, 39, 40, 43, 44, 45, 47)
thirtyeight_tranche_docs <- c(2113:2329)
allocate_randomly(user_ids = thirtyeight_coding_group,
                  set = thirtyeight_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 39
thirtynine_double_coding_group <- c(20)
thirtynine_single_coding_group <- c(19, 21, 38)
thirtynine_tranche_docs <- c(2330:2380)
allocate_randomly_with_some_double_assignment(double_coding_user_ids = thirtynine_double_coding_group,
                                              other_user_ids = thirtynine_single_coding_group,
                                              set = thirtynine_tranche_docs,
                                              n_double_coding = 20,
                                              coder_rate = 1.1,
                                              allocation_type = "coding",
                                              allocated_by = "irregular_random_assignment",
                                              restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 40
forty_coding_group <- c(8, 16, 31, 34, 36, 47)
forty_tranche_docs <- c(2381:2500)
allocate_randomly(user_ids = forty_coding_group,
                  set = forty_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 41
fortyone_coding_group <- c(10, 15, 19, 23, 27, 29, 38, 41)
fortyone_tranche_docs <- c(2501:2564)
allocate_randomly(user_ids = fortyone_coding_group,
                  set = fortyone_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 42
fortytwo_coding_group <- c(39, 12, 9, 31, 38)
fortytwo_tranche_docs <- c(2565:2660)
allocate_randomly(user_ids = fortytwo_coding_group,
                  set = fortytwo_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)


# Random allocation 43
fortythree_coding_group <- c(8, 19, 21, 22)
fortythree_tranche_docs <- c(2661:2781)
allocate_randomly(user_ids = fortythree_coding_group,
                  set = fortythree_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 44
fortyfour_coding_group <- c(41, 40)
fortyfour_tranche_docs <- c(2782:2838)
allocate_randomly(user_ids = fortyfour_coding_group,
                  set = fortyfour_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 45
fortyfive_coding_group <- c(8, 11, 24, 27, 29, 34, 35, 36, 37, 47)
fortyfive_tranche_docs <- c(2839:2989)
allocate_randomly(user_ids = fortyfive_coding_group,
                  set = fortyfive_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 46
fortysix_coding_group <- c(20, 24)
fortysix_tranche_docs <- c(2990:3020)
allocate_randomly(user_ids = fortysix_coding_group,
                  set = fortysix_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 47
fortyseven_coding_group <- c(34, 41)
fortyseven_tranche_docs <- c(3021:3055)
allocate_randomly(user_ids = fortyseven_coding_group,
                  set = fortyseven_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 48
fortyeight_coding_group <- c(8, 9, 11, 16, 20, 21, 22, 23, 24, 25, 28, 31, 35, 42, 43, 44, 45)
fortyeight_tranche_docs <- c(3056:3311)
allocate_randomly(user_ids = fortyeight_coding_group,
                  set = fortyeight_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 49
fortynine_coding_group <- c(8, 20, 22, 25, 31, 42)
fortynine_tranche_docs <- c(3312:3440)
allocate_randomly(user_ids = fortynine_coding_group,
                  set = fortynine_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 50
fifty_coding_group <- c(8, 16)
fifty_tranche_docs <- c(3441:3500)
allocate_randomly(user_ids = fifty_coding_group,
                  set = fifty_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 51
fifty_coding_group <- c(12, 15, 20, 22, 23, 24, 25, 27, 28, 29, 34, 35, 36, 37, 38, 41, 42, 43, 44, 45, 47)
fifty_tranche_docs <- c(3501:3608)
allocate_randomly(user_ids = fifty_coding_group,
                  set = fifty_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 52
fiftytwo_coding_group <- c(9, 12, 15, 20, 21, 22, 23, 24, 25, 27, 28, 29, 34, 35, 36, 37, 38, 41, 42, 43, 44, 45, 46, 47)
fiftytwo_tranche_docs <- c(3609:3750)
allocate_randomly(user_ids = fiftytwo_coding_group,
                  set = fiftytwo_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 53
fiftythree_coding_group <- c(22, 27)
fiftythree_tranche_docs <- c(3751:3775)
allocate_randomly(user_ids = fiftythree_coding_group,
                  set = fiftythree_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 54
fiftyfour_coding_group <- c(17, 29)
fiftyfour_tranche_docs <- c(3776:3810)
allocate_randomly(user_ids = fiftyfour_coding_group,
                  set = fiftyfour_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 55
fiftyfive_coding_group <- c(9, 12, 21, 21, 23, 24, 27, 34, 35, 36, 38, 41, 42, 43, 44, 47)
fiftyfive_tranche_docs <- c(3811:3859)
allocate_randomly(user_ids = fiftyfive_coding_group,
                  set = fiftyfive_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 56
fiftysix_coding_group <- c(9, 12, 20, 23, 24, 27, 35, 36, 38, 41, 42, 47)
fiftysix_tranche_docs <- c(3860:4013)
allocate_randomly(user_ids = fiftysix_coding_group,
                  set = fiftysix_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 57
fiftyseven_coding_group <- c(20, 23, 24, 41)
fiftyseven_tranche_docs <- c(4014:4030)
allocate_randomly(user_ids = fiftyseven_coding_group,
                  set = fiftyseven_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 58
fiftyeight_coding_group <- c(20, 23, 27, 41)
fiftyeight_tranche_docs <- c(4031:4075)
allocate_randomly(user_ids = fiftyeight_coding_group,
                  set = fiftyeight_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 59
fiftynine_coding_group <- c(15, 20, 21, 22, 23, 35, 36, 41, 42, 47)
fiftynine_tranche_docs <- c(4076:4136)
allocate_randomly(user_ids = fiftynine_coding_group,
                  set = fiftynine_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 60
sixty_double_coding_group <- c(48)
sixty_single_coding_group <- c(15, 20, 21, 22, 23, 36, 41, 43, 45, 46, 47)
sixty_tranche_docs <- c(4136:4290)
allocate_randomly_with_some_double_assignment(double_coding_user_ids = sixty_double_coding_group,
                                              other_user_ids = sixty_single_coding_group,
                                              set = sixty_tranche_docs,
                                              n_double_coding = 20,
                                              coder_rate = 1.1,
                                              allocation_type = "coding",
                                              allocated_by = "irregular_random_assignment",
                                              restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 61
sixtyone_coding_group <- c(9, 24, 26, 29, 31, 35, 42, 43)
sixtyone_tranche_docs <- c(4291:4360)
allocate_randomly(user_ids = sixtyone_coding_group,
                  set = sixtyone_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 62
sixtyone_coding_group <- c(24, 31, 39)
sixtyone_tranche_docs <- c(4361:4391)
allocate_randomly(user_ids = sixtyone_coding_group,
                  set = sixtyone_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 63
sixtythree_coding_group <- c(24, 39)
sixtythree_tranche_docs <- c(4392:4432)
allocate_randomly(user_ids = sixtythree_coding_group,
                  set = sixtythree_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 64
sixtyfour_coding_group <- c(24, 40, 47)
sixtyfour_tranche_docs <- c(4433:4468)
allocate_randomly(user_ids = sixtyfour_coding_group,
                  set = sixtyfour_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)

# Random allocation 65
sixtyfive_coding_group <- c(24, 29)
sixtyfive_tranche_docs <- c(4469:4505)
allocate_randomly(user_ids = sixtyfive_coding_group,
                  set = sixtyfive_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)


# Random allocation 66
sixtysix_coding_group <- c(8, 9, 19, 22, 24, 25, 27, 28, 36, 42)
sixtysix_tranche_docs <- c(4506:4606)
allocate_randomly(user_ids = sixtysix_coding_group,
                  set = sixtysix_tranche_docs, coder_rate = 1.1,
                  allocation_type = "coding", allocated_by = "regular_random_assignment",
                  restrict_to_actual = TRUE, make_assignments = TRUE)
