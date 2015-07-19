concrete Hsk1Chi of Hsk1 =
-- syntax
  CatChi [
    Utt,S,Cl,QS,QCl,NP,VP,VPSlash,CN,AP,Pron,Det,Quant,Conj,N,A,Adv,AdA,V,V2,VV,Temp
    ],
  NounChi [
    NP,Det,CN,Pron,PN,Quant,Num,Card,Numeral,AP,N,
    DetCN,UsePN,UsePron,DetQuant,NumSg,NumPl,NumCard,NumNumeral,IndefArt,DefArt,MassNP,PossPron,UseN,AdjCN,AdjCN,PossNP
    ],
  AdjectiveChi [
    AdA,AP,A,
    PositA,AdAP
    ],
  AdverbChi [
    NP,Prep,Adv,
    PrepNP
    ],
  VerbChi [
    V,VS,V2,VV,VP,VPSlash,NP,Adv,Comp,AP,S,
    UseV,ComplVS,ComplVV,SlashV2a,ComplSlash,AdvVP,CompAdv,CompAP,UseComp
    ],
  SentenceChi [
    S,Cl,QCl,Temp,Pol,NP,VP,QS,ClSlash,VPSlash,
    PredVP,UseCl, UseQCl,SlashVP
    ],
  TenseX [
    TTAnt,PPos,PNeg,TPres,ASimul,AAnter
    ],
  QuestionChi [
    QCl,Cl,VP,VPSlash,ClSlash,IP,IDet,IQuant,IComp,IAdv,NP,CN,Num,Prep,Tense,
    QuestCl, QuestVP, QuestSlash,QuestIAdv,QuestComp,IdetCN,IdetQuant,PrepIP,CompIAdv,CompIP
    ],
  PhraseChi [
   Utt,S,QS,
   UttS,UttQS
   ],



-- lexicon
  NumeralChi - [pot2,pot2plus,pot3,pot3plus]  -- from 1 to 99

  -- Lexicon, Structural made explicit to maintain Hsk wordings and order


 ** open SyntaxChi, ParadigmsChi, (R=ResChi), Prelude in {

lin i_Pron = R.pronNP "我" ;
lin youSg_Pron = R.pronNP "你" ;
lin in_Prep = mkPrep "在" ;
lin he_Pron = R.pronNP "他" ;
lin we_Pron = R.pronNP "我们" ;
lin good_A = mkA "好" ;
lin have_V2 = mkV2 "有" ;
lin this_Quant = mkQuant "这" ;
lin can8know_VV = mkVV "会" ;
lin whatSg_IP = R.ssword "什么" ;
lin speak_V2 = mkV2 "说" ;
lin she_Pron = R.pronNP "她" ;
lin think_VS = mkVS "想" ;
lin very_AdA = mkAdA "很" ;
lin man_N = mkN "人" "個" ;
lin person_N = mkN "人" "個" ;
lin that_Quant = mkQuant "那" ;
lin come_V = mkV "来" ;
lin next_A = mkA "来" ;
lin all_Det = R.mkDet "都" ;
lin both_Det = R.mkDet "都" ;
lin can_VV = mkVV "能" ;
lin go_V2 = mkV2 "去" ;
lin leave_V = mkV "去" ;
lin and_Conj = mkConj "和" ;
lin do_V2 = mkV2 "做" ;
lin above_Prep = mkPrep "上" "" placeAdvType ;
lin up_Adv = mkAdv "上" placeAdvType ;
lin see_V2 = mkV2 "看" | mkV2 "看见" ;
lin look_V2 = mkV2 "看" ;
lin watch_V2 = mkV2 "看" ;
lin how_IAdv = R.ssword "怎么" | R.ssword "哪" ;
lin now_Adv = mkAdv "现在" timeAdvType ;
lin oclock_Adv card = lin Adv {s = card.s ++ "点" ; advType = timeAdvType} ;
lin how_about_Utt np = lin Utt (ss (np.s ++ "呢")) | lin Utt (ss (np.s ++ "怎么样")) ;
lin too_AdA = mkAdA "太" ;
lin inside_Adv = mkAdv "里" placeAdvType ;
lin listen_V2 = mkV2 "听" ;
lin hear_V2 = mkV2 "听" ;
lin whoSg_IP = R.ssword "谁" ;
lin many_Det = R.mkDet "多" ;
lin time_N = mkN "时候" "个" ;
lin fall_V = mkV "下" ;
lin below_Prep = mkPrep "下" "" placeAdvType ;
lin thank_you_Utt = lin Utt (ss "谢谢") ;
lin mr_NP pn = lin NP (ss (pn.s ++ "先生")) ;
lin like_V2 = mkV2 "喜欢" ;
lin big_A = mkA "大" ;
lin large_A = mkA "大" ;
lin stuff_N = mkN "东西" "個" ;
lin small_A = mkA "小" ;
lin young_A = mkA "小" ;
lin have_name_Cl np pn = mkCl np (mkV2 "叫") (mkNP pn) ;
lin love_V2 = mkV2 "爱" ;
lin year_N = mkN "年" "個" ;
lin please_Utt = lin Utt (ss "请") ;
lin invite_V2 = mkV2 "请" ;
lin return_V = mkV "回" ;
lin reply_V = mkV "回" ;
lin work_V = mkV "工作" ;
lin job_N = mkN "工作" "個" ;
lin coin_N = mkN "钱" "筆" ;
lin eat_V2 = mkV2 "吃" ;
lin open_V2 = mkV2 "开" ;
lin start_VV = mkVV "开" ;
lin family_N = mkN "家" "个" ;
lin home_N = mkN "家" "个" ;
lin which_IQuant = mkIQuant "哪" ;
lin friend_N = mkN "朋友" "個" ;
lin mom_N = mkN "妈妈" "個" ;
lin today_Adv = mkAdv "今天" timeAdvType ;
lin several_Det = R.mkDet "些" | R.mkDet "几" ;
lin dad_N = mkN "爸爸" "個" ;
lin some_Quant = mkQuant "些" ;
lin a_few_Det = R.mkDet "些" | R.mkDet "几" ;
lin sorry_Utt = lin Utt (ss "对不起") ;
lin live_V = mkV "住" ;
lin reside_V = mkV "住" ;
lin happy_A = mkA "高兴" ;
lin glad_A = mkA "高兴" ;
lin buy_V2 = mkV2 "买" ;
lin doctor_N = mkN "医生" "名" ;
lin where_IAdv = R.ssword "哪儿" ;
lin letter_character_N = mkN "字" "個" ;
lin name_N = mkN "名字" "个" ;
lin recognize_V2 = mkV2 "认识" ;
lin know_V2 = mkV2 "认识" ;
lin sit_V = mkV "坐" ;
lin drink_V2 = mkV2 "喝" ;
lin write_V2 = mkV2 "写" ;
lin moon_N = mkN "月" "個" ;
lin month_N = mkN "月" "個" ;
lin number_N = mkN "号" ;
lin monthday_N = mkN "号" ;
lin dog_N = mkN "狗" "隻" ;
lin years_old_AP card = mkAP (lin A (ss (card.s ++ "岁"))) ;
lin age_N = mkN "岁" "个" ;
lin telephone_V = mkV "打电话" ;
lin hello_Utt = lin Utt (ss "喂") ;
lin son_N = mkN "儿子" "个" ;
lin pretty_A = mkA "漂亮" ;
lin beautiful_A = mkA "漂亮" ;
lin minute_N = mkN "分钟" "个" ;
lin goodbye_Utt = lin Utt (ss "再见") ;
lin book_N = mkN "书" | mkN "本" "本" ;
lin tomorrow_Adv = mkAdv "明天" timeAdvType ;
lin few_Det = R.mkDet "少" ;
lin little_Det = R.mkDet "少" ;
lin how8much_IAdv = lin IAdv (R.ssword "多少") ;
lin how8many_IDet = mkIDet "多少" | mkIDet "几" ;
lin piece_N = mkN "块" "个" ;
lin money_NP = mkNP (mkN "块") ;
lin daughter_N = mkN "女儿" "个" ;
lin lady_N = mkN "小姐" "個" ;
lin ms_NP pn = lin NP (ss (pn.s ++ "小姐")) ;
lin clothes_N = mkN "衣服" "件" ;
lin water_N = mkN "水" "滴" ;
lin school_N = mkN "学校" "所" ;
lin movie_N = mkN "电影" "部" ;
lin film_N = mkN "电影" "部" ;
lin letter_document_N = mkN "书" "本" ;
lin hospital_N = mkN "医院" "所" ;
lin never_mind_Utt = lin Utt (ss "没关系") ;
lin airplane_N = mkN "飞机" "架" ;
lin television_N = mkN "电视" "臺" ;
lin read_V2 = mkV2 "读" ;
lin study_V2 = mkV2 "读" ;
lin back_Adv = mkAdv "后面" placeAdvType ;
lin behind_Prep = mkPrep "后面" "" placeAdvType ;
lin yesterday_Adv = mkAdv "昨天" timeAdvType ;
lin sleep_V = mkV "睡觉" ;
lin teacher_N = mkN "老师" "名" ;
lin week_N = mkN "星期" "個" ;
lin cat_N = mkN "猫" "隻" ;
lin computer_N = mkN "电脑" "臺" ;
lin heat_N = mkN "热" "个" ;
lin hot_A = mkA "热" ;
lin student_N = mkN "学生" "名" ;
lin afternoon_N = mkN "下午" "個" ;
lin learn_V = mkV "学习" ;
lin study_V = mkV "学习" ;
lin cold_A = mkA "冷" ;
lin youre_welcome_Utt = lin Utt (ss "不客气") ;
lin in_front_Adv = mkAdv "前面" placeAdvType ;
lin in8front_Prep = mkPrep "前面" "" placeAdvType ;
lin china_PN = mkPN "中国" ;
lin dish_N = mkN "菜" "盤" ;
lin vegetable_N = mkN "菜" "盤" ;
lin table_N = mkN "桌子" "張" ;
lin desk_N = mkN "桌子" "張" ;
lin taxi_N = mkN "出租车" "辆" ;
lin weather_N = mkN "天气" "个" ;
lin tea_N = mkN "茶" "杯" ;
lin shop_N = mkN "商店" "间" ;
lin chair_N = mkN "椅子" "把" ;
lin schoolmate_N = mkN "同学" "名" ;
lin a_bit_Det = R.mkDet "一点儿" ;
lin apple_N = mkN "苹果" "個" ;
lin restaurant_N = mkN "饭店" "间" ;
lin noon_N = mkN "中午" "個" ;
lin late_morning_N = mkN "上午" "個" ;
lin fruit_N = mkN "水果" "個" ;
lin cup_N = mkN "杯子" "個" ;
lin rain_V0 = mkV "下雨" ;
lin cooked_rice_N = mkN "米饭" "个" ;
lin beijing_PN = mkPN "北京" ;
lin chinese_PN = mkPN "汉语" ;
lin wang_PN = mkPN "王" ;

}


