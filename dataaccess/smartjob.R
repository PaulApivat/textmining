# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# download from smart_job_data.csv (see Notion project notes; i.e., Department of Employment)

# package
library(tidyverse)
library(geofacet)    # visualize geospatial (province) data
library(formattable)  # nicely format province_salary2 table

# load and save data
load(file = "saku.RData")

# read smart_job_data.csv
smart_job_data <- read.csv("smart_job_data.csv")

# quick exploratory
# data SHAPE: 5,944 rows, 175 columns
summary(smart_job_data)

# Unique UserID:
# data SHAPE: 2,956 rows, 175 columns
View(distinct(smart_job_data, UserID, .keep_all = TRUE))

# check column names (compare to data_full)
names(smart_job_data)  # compare names(data_full)

## NOTES
# - jobseeker can create multiple entries in smart_job_data (see UserID)

##### Subset data based on unique UserID #####
# data SHAPE: 2,956 rows, 175 columns
distinct(smart_job_data, UserID, .keep_all = TRUE) -> smartjob_unique

smartjob_unique

##### NOTE some UserID had multiple entries
## 188338 had 17 entries (most), 14798488 had 15 entries (second) etc.

####### Exploratory Data Visualization (Descriptive) ##########
# note: assumption of unique UserID
# note: each unique UserID had corresponding unique EmployerID (NO overlap in jobs?)
# note: will try not to create additional data frames aside from smartjob_unique

### Employment Types Sought
smartjob_unique %>% 
    group_by(EmploymentType) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x = reorder(EmploymentType, n), y=n, fill=EmploymentType)) 
        + geom_bar(stat = 'identity') 
        + geom_text(aes(label=n), vjust=-0.5) 
        # changing Legend text labels
        + scale_fill_discrete(name = "Employment Type", 
                              labels = c("Part-Time", "Full-Time", "Temporary")) 
        + labs(x = 'Employment Type', 
               y = 'Number of People', 
               title = "Smart Job Data: Job Seeker by Employment Type")

### Applicant Types of Job Seekers
smartjob_unique %>% 
    group_by(ApplicantTypeName) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x = reorder(ApplicantTypeName, n), y=n, fill=ApplicantTypeName)) 
        + geom_bar(stat = 'identity') 
        + geom_text(aes(label=n), vjust=-0.5) 
        + scale_fill_discrete(name = "Applicant Type Name", labels = c("Disability m.33", "Elder", "General Job Seeker")) 
        + labs(x = 'Applicant Type Name', y = 'Number of People', title = "Smart Job Data: Job Seeker by Applicant Type") 
        + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust = 1))


#### Top 24 Job Descriptions ranked and grouped by Job Fields
smartjob_unique %>% 
    group_by(JobDescription, JobFieldName) %>% 
    tally(sort = TRUE) %>% 
    filter(n > 3) %>% 
    filter(JobDescription !='-') %>% 
    ggplot(aes(x=reorder(JobDescription,n), y=n, fill=JobFieldName)) 
        + geom_bar(stat = 'identity') 
        + geom_text(aes(label=n), vjust=-0.5) 
        + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust = 1), 
                legend.text = element_text(family = 'Krub')) 
        + labs(x = 'Job Description', 
               y = 'Number of People', 
               title = 'Smart Job: Top 24 Job Descriptions ranked and grouped by Job Fields')

#### Top 30 Job Positions 
smartjob_unique %>% 
    group_by(JobPosition) %>% 
    tally(sort = TRUE) %>% 
    # filtering only top 30 job positions
    filter(n > 11) %>% 
    ggplot(aes(x=reorder(JobPosition,desc(n)), y=n)) 
        + geom_bar(stat = 'identity') 
        + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust = 1)) 
        + labs(x = 'Job Position', 
               y = 'Number of People', 
               title = "Smart Job: Applicant's Top 30 Job Positions")

#### NOTE:
# without coord_flip(): ggplot(aes(x=reorder(x,desc(n))), axis.text.x = element_text(angle = 45) + geom_text(vjust = -0.5)
# with coord_flip(): ggplot(aes(x=reorder(x,n))), axis.text.y = element_text() + geom_text(hjust = -0.5)
# with coord_flip(): vjust -> hjust, axis.text.x -> axis.text.y, angle -> "no" angle needed

#### Top 30 Job Position NAMES
smartjob_unique %>% 
    group_by(JobPositionName) %>% 
    tally(sort = TRUE) %>% 
    filter(n > 19) %>% 
    ggplot(aes(x=reorder(JobPositionName,n), y=n)) 
        + geom_bar(stat = 'identity') 
        + theme(axis.text.y = element_text(family = 'Krub', hjust = 1)) 
        + labs(x = 'Job Position NAME', 
               y = 'Number of People', 
               title = 'Smart Job: Top 30 Job Positions NAMES') 
        + geom_text(aes(label=n), hjust=-0.5) 
        + coord_flip()

##### VISUALIZE INFORMATION VIA PROVINCE #####
library(geofacet)

mygrid <- data.frame(
  row = c(2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 10, 10, 10, 11, 11, 12, 13, 14, 15, 16, 17, 17, 17, 18, 19, 20, 20, 20, 21, 21),
  col = c(3, 2, 3, 4, 1, 9, 8, 2, 5, 3, 2, 10, 9, 8, 7, 6, 5, 4, 9, 2, 8, 3, 4, 6, 7, 5, 10, 11, 3, 9, 8, 6, 7, 4, 5, 10, 8, 4, 3, 6, 7, 9, 5, 9, 3, 2, 5, 7, 8, 4, 6, 3, 4, 7, 8, 5, 6, 4, 7, 8, 3, 9, 3, 3, 3, 3, 3, 4, 5, 3, 5, 5, 7, 5, 6, 8, 7),
  name = c("Chiang Rai", "Chiang Mai", "Phayao", "Nan", "Mae Hong Son", "Bungkan", "Nong Khai", "Lampang", "Uttaradit", "Phrae", "Lamphun", "Nakhon Phanom", "Sakon Nakhon", "Udon Thani", "Nong Bua Lam Phu", "Loei", "Phitsanulok", "Sukhothai", "Mukdahan", "Tak", "Kalasin", "Kamphaeng Phet", "Phichit", "Chaiyaphum", "Khon Kaen", "Phetchabun", "Amnat Charoen", "Ubon Ratchathani", "Nakhon Sawan", "Roi Et", "Maha Sarakham", "Saraburi", "Lop Buri", "Sing Buri", "Ang Thong", "Yasothon", "Si Sa Ket", "Chai Nat", "Uthai Thani", "Pathum Thani", "Nakhon Ratchasima", "Buri Ram", "Nonthaburi", "Surin", "Suphan Buri", "Kanchanaburi", "Bangkok", "Nakhon Nayok", "Prachin Buri", "Phra Nakhon Si Ayutthaya", "Samut Prakan", "Ratchaburi", "Nakhon Pathom", "Chachoengsao", "Sa Kaeo", "Samut Sakhon", "Chon Buri", "Samut Songkhram", "Rayong", "Chanthaburi", "Phetchaburi", "Trat", "Prachuap Khiri Khan", "Chumphon", "Ranong", "Surat Thani", "Phangnga", "Krabi", "Nakhon Si Thammarat", "Phuket", "Trang", "Phattalung", "Pattani", "Satun", "Songkhla", "Narathiwat", "Yala"),
  code = c("CRI", "CMI", "PYO", "NAN", "MSN", "BKN", "NKI", "LPG", "UTD", "PRE", "LPN", "NPM", "SNK", "UDN", "NBP", "LEI", "PLK", "STI", "MDH", "TAK", "KSN", "KPT", "PCT", "CPM", "KKN", "PNB", "ACR", "UBN", "NSN", "RET", "MKM", "SRI", "LRI", "SBR", "ATG", "YST", "SSK", "CNT", "UTI", "PTE", "NMA", "BRM", "NBI", "SRN", "SPB", "KRI", "BKK", "NYK", "PRI", "AYA", "SPK", "RBR", "NPT", "CCO", "SKW", "SKN", "CBI", "SKM", "RYG", "CTI", "PBI", "TRT", "PKN", "CPN", "RNG", "SNI", "PNA", "KBI", "NRT", "PKT", "TRG", "PLG", "PTN", "STN", "SKA", "NWT", "YLA"),
  stringsAsFactors = FALSE
)

grid_preview(mygrid)

# add new column ProvinceNameEn after ProvinceName in smartjob_unique ###
smartjob_unique <- add_column(smartjob_unique, ProvinceNameEn = NA, .after = "ProvinceName")

# conditionally change thai province names to English

> smartjob_unique <- add_column(smartjob_unique, ProvinceNameEn = NA, .after = "ProvinceName")
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='กรุงเทพมหานคร', 'Bangkok', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สงขลา', 'Songkhla', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ปทุมธานี', 'Pathum Thani', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นนทบุรี', 'Nonthaburi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ชลบุรี', 'Chon Buri', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สมุทรปราการ', 'Samut Prakan', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ปราจีนบุรี', 'Prachin Buri', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='เชียงใหม่', 'Chiang Mai', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สมุทรสาคร', 'Samut Sakhon', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ภูเก็ต', 'Phuket', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ประจวบคีรีขันธ์', 'Prachuap Khiri Khan', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ฉะเชิงเทรา', 'Chachoengsao', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='เชียงราย', 'Chiang Rai', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นครพนม', 'Nakhon Phanom', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สุรินทร์', 'Surin', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ขอนแก่น', 'Khon Kaen', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นครปฐม', 'Nakhon Pathom', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='มุกดาหาร', 'Mukdahan', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ลำพูน', 'Lamphun', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ชุมพร', 'Chumphon', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นครศรีธรรมราช', 'Nakhon Si Thammarat', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='พัทลุง', 'Phattalung', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ราชบุรี', 'Ratchaburi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ระยอง', 'Rayong', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ร้อยเอ็ด', 'Roi Et', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ลำปาง', 'Lampang', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='อุบลราชธานี', 'Ubon Ratchathani', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='มหาสารคาม', 'Maha Sarakham', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นครราชสีมา', 'Nakhon Ratchasima', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='พิษณุโลก', 'Phitsanulok', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ลพบุรี', 'Lop Buri', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สุราษฎร์ธานี', 'Surat Thani', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='กำแพงเพชร', 'Kamphaeng Phet', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นราธิวาส', 'Narathiwat', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='อุดรธานี', 'Udon Thani', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='กระบี่', 'Krabi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='พระนครศรีอยุธยา', 'Phra Nakhon Si Ayutthaya', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สระบุรี', 'Saraburi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สุพรรณบุรี', 'Suphan Buri', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='กาญจนบุรี', 'Kanchanaburi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='หนองคาย', 'Nong Khai', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='เพชรบูรณ์', 'Phetchabun', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='กาฬสินธุ์', 'Kalasin', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ระนอง', 'Ranong', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สิงห์บุรี', 'Sing Buri', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ตรัง', 'Trang', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='น่าน', 'Nan', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='บึงกาฬ', 'Bungkan', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='บุรีรัมย์', 'Buri Ram', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ยะลา', 'Yala', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ยโสธร', 'Yasothon', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='จันทบุรี', 'Chanthaburi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ชัยนาท', 'Chai Nat', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ตราด', 'Trat', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='พะเยา', 'Phayao', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สุโขทัย', 'Sukhothai', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นครสวรรค์', 'Nakhon Sawan', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ปัตตานี', 'Pattani', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สกลนคร', 'Sakon Nakhon', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สมุทรสงคราม', 'Samut Songkhram', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='อุตรดิตถ์', 'Uttaradit', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='เพชรบุรี', 'Phetchaburi', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='เลย', 'Loei', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='อำนาจเจริญ', 'Amnat Charoen', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='อุทัยธานี', 'Uthai Thani', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='นครนายก', 'Nakhon Nayok', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='แพร่', 'Phrae', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สระแก้ว', 'Sa Kaeo', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ศรีสะเกษ', 'Si Sa Ket', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='อ่างทอง', 'Ang Thong', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='แม่ฮ่องสอน', 'Mae Hong Son', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='สตูล', 'Satun', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='หนองบัวลำภู', 'Nong Bua Lam Phu', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='พังงา', 'Phangnga', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ตาก', 'Tak', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='ชัยภูมิ', 'Chaiyaphum', smartjob_unique$ProvinceNameEn)
> smartjob_unique$ProvinceNameEn <- ifelse(smartjob_unique$ProvinceName=='พิจิตร', 'Phichit', smartjob_unique$ProvinceNameEn)

# create custom grid mygrid with Thai alphabet
smartjob_unique %>% group_by(ProvinceName, ProvinceNameEn) %>% tally(sort = TRUE) -> mygrid2

colnames(mygrid2)[2] <- "name"

# join with mygrid to get 'row' and 'col'
mygrid2 <- mygrid %>%
    inner_join(mygrid2, by = 'name')


### APPLICANT BY PROVINCE ###
# note: to filter out Bangkok (outlier); mygrid2 %>% filter(code != 'BKK') 

ggplot(data = mygrid2, mapping = aes(xmin = col, ymin = row, xmax = col + 1, ymax = row + 1, fill = n)) 
    + geom_rect(color = '#ffffff') 
    + theme_minimal() 
    + theme(panel.grid = element_blank(), 
            axis.text = element_blank(), 
            axis.title = element_blank()) 
    + geom_text(aes(x = col, y = row, label = ProvinceName), 
                family = 'Krub', 
                alpha = 0.5, 
                nudge_x = 0.5, 
                nudge_y = -0.5, 
                size = 3) 
    + scale_y_reverse() 
    + scale_fill_gradient2(low = '#ffeda0', 
                           mid = '#feb24c', 
                           high = '#f03b20', 
                           midpoint = 100, 
                           na.value = 'white', 
                           guide = 'colourbar', 
                           aesthetics = 'fill') 
    + labs(fill = 'Applicants', 
           title = 'Job Applicants by Province', 
           subtitle = 'Bangkok is an outlier') 

#####----------------- Other Categories (To Do) -------------- ######
# - SalaryRequireUnitName
smartjob_unique %>% group_by(SalaryRequireUnitName) %>% tally(sort = TRUE)
# - TypeName
smartjob_unique %>% group_by(TypeName) %>% tally(sort = TRUE)

## Mininum Wage
# Need to create brackets - too many rows
smartjob_unique %>% group_by(Wage_Min) %>% tally(sort = TRUE)



###### ----------------Explain How Jobs Expire ------------------######
smartjob_unique %>% group_by(ExpireDate) %>% tally(sort = TRUE) -> expire_date

expire_date[order(as.Date(expire_date$ExpireDate, format="%d/%m/%Y")),] -> expire_date2

expire_date2

### Highlight Expiration Date on Jobs, relative to "today" (June 12, 2020)
ggplot(data = expire_date2, mapping = aes(x=as.Date(ExpireDate, format="%d/%m/%Y"), y=n)) 
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10)) 
    # vertical line for today, entered manually
    + geom_vline(xintercept = as.numeric(as.Date("2563-06-12")), linetype=4, color = 'red') 
    + geom_text(aes(label=n), nudge_x = 1, nudge_y = -1)


# alternative way to do geom_vline()
ggplot(data = expire_date2, mapping = aes(x=as.Date(ExpireDate, format="%d/%m/%Y"), y=n)) 
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10)) 
    # more intuitive way to enter date
    + geom_vline(xintercept = as.numeric(as.Date("12/06/2563", format="%d/%m/%Y")), linetype=4, color = 'red') 
    + geom_text(aes(label=n), nudge_x = 1, nudge_y = -1) 
    + labs(x="Job Post Expiration Dates", y="Number of Posting", title="Job Posting: May 7th - Aug 6th, 2020", subtitle = "Today: June 12, 2020")



###### ----------------[User ใช้เวลาเฉลี่ยเท่าไร] ------------------######
# UNSURE

###### ----------------[User แต่ละที่ เรียกเงินเดือนกันเท่าไร] ------------------######

# Filter by TypeName (wage type)
# group_by province
# summarize average minimum salary

# first convert Wage_Min from factor to numeric/integer
smartjob_unique <- add_column(smartjob_unique, Wage_Min_num = NA, .after = "Wage_Min")

### Formatting Issues: 10,000.00 and 320 in the same column use gsub()
smartjob_unique$Wage_Min_num <- as.numeric(gsub(",", "", smartjob_unique$Wage_Min))

### Long table of ProvinceName by SalaryRequireUnitName (need to WIDEN it)
### principle of Tidy Tables
smartjob_unique %>% 
    group_by(ProvinceName, SalaryRequireUnitName) %>% 
    summarize(avg_min_wage = mean(Wage_Min_num)) -> province_salary

province_salary

### Spread Table (province_salary2)
### principle of Tidy Tables
spread(province_salary, key = SalaryRequireUnitName, value = avg_min_wage) -> province_salary2

province_salary2

# change ALL numbers in dataframe to two decimal places
province_salary2[,2:4] <- round(province_salary2[,2:4], digits = 2)

## Formatting province_salary2 table
library(formattable)

formattable(province_salary2)  # bare bones

## Add green color
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

formattable(province_salary2, 
            align = c("l", "c", "c", "c"), 
            list(`ProvinceName` = formatter("span", style = style(color = "grey", font.weight = "bold")), 
            `ชั่วโมง`= color_tile(customGreen, customGreen0), 
            `เดือน`= color_tile(customGreen, customGreen0), 
            `วัน`= color_tile(customGreen, customGreen0)))



##### REQUIRE MANYCHAT or MIXPANEL or POSTGRESQL access [User ที่กดเข้ามามีแนวโน้มที่จะใช้ตัวเลือก หาคน หางาน หรือ ไม่เลือกตัวเลือกใด ๆ เลยเป็นเท่าไร] #######
##### REQUIRE MANYCHAT or MIXPANEL or POSTGRESQL access [User กี่ % ที่กดเข้าไปดูงานที่เราส่งไปให้จริง ๆ] ######
##### REQUIRE MANYCHAT or MIXPANEL or POSTGRESQL access [User หยุดหรือใช้เวลาที่ flow ไหนนานสุด] #######

##### SEE GSHEET - JOB SEEKER BY JOB CATEGORY [User ที่หางานส่วนใหญ่จะเลือกค้นหางานประเภทไหน] ######

