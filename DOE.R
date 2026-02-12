# หากยังไม่มีให้ติดตั้ง: install.packages("ggtern")
library(ggtern)

# 1. เตรียมข้อมูล
df <- data.frame(
  ACN = c(13, 11, 15, 13),
  MeOH = c(19, 19, 19, 17),
  Buffer = c(68, 70, 66, 70),
  RT = c(17.444, 22.252, 13.283, 18.804)
)

# 2. วาดกราฟ Contour
ggtern(data = df, aes(x = ACN, y = MeOH, z = Buffer)) +
  stat_interpolate_tern(aes(value = RT, fill = ..level..), 
                        geom = "polygon", 
                        method = lm, formula = value ~ x + y) +
  geom_point(size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Retention Time Response Surface",
       fill = "RT (min)") +
  theme_rgbw()

# 1. ติดตั้งและเรียกใช้
if (!require(ggtern)) install.packages("ggtern")
library(ggtern)

# 2. เตรียมข้อมูล (ใช้ค่าจริงได้เลย)
df <- data.frame(
  ACN = c(13, 11, 15, 13),
  MeOH = c(19, 19, 19, 17),
  Buffer = c(68, 70, 66, 70),
  RT = c(17.444, 22.252, 13.283, 18.804)
)

# 3. วาดกราฟ
ggtern(data = df, aes(x = ACN, y = MeOH, z = Buffer)) +
  # สร้างพื้นผิวจากการประมาณค่า (Interpolation)
  stat_interpolate_tern(aes(value = RT, fill = ..level..), 
                        geom = "polygon", 
                        method = "lm", # ใช้ Linear model ตามจำนวนจุดที่มี
                        n = 100) +    # ความละเอียดของพื้นที่
  # วาดจุดทดลองจริงลงไป
  geom_point(size = 4, color = "black") +
  # ใส่ตัวเลขค่า RT กำกับที่จุด
  geom_text(aes(label = RT), vjust = -1, fontface = "bold") +
  # กำหนดสี (น้ำเงิน = RT ต่ำ/เร็ว, แดง = RT สูง/ช้า)
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "RSM for Retention Time",
       subtitle = "Mobile Phase: ACN/MeOH/Buffer",
       fill = "RT (min)") +
  theme_rgbw() +
  # ขยายพิกัด (Zoom) เข้าไปในจุดที่ทดลองเพื่อไม่ให้เห็นสามเหลี่ยมกว้างเกินไป
  limit_tern(T = 0.25, L = 0.20, R = 0.75)

model <- lm(RT ~ ACN + MeOH + Buffer - 1, data = df)
summary(model)


# 1. เตรียมข้อมูลและสร้าง Model
df <- data.frame(
  ACN = c(13, 11, 15, 13),
  MeOH = c(19, 19, 19, 17),
  Buffer = c(68, 70, 66, 70),
  RT = c(17.444, 22.252, 13.283, 18.804)
)

# สร้าง Linear Mixture Model (ไม่มี Intercept)
fit <- lm(RT ~ ACN + MeOH + Buffer - 1, data = df)

# 2. ฟังก์ชันสำหรับหาความเข้มข้น ACN เมื่อทราบค่าเป้าหมาย RT และค่า MeOH ที่ต้องการล็อคไว้
find_mobile_phase <- function(target_rt, fixed_meoh) {
  # สมการ: RT = b1(ACN) + b2(MeOH) + b3(Buffer)
  # แทนค่า Buffer = 100 - ACN - fixed_meoh
  # RT = b1(ACN) + b2(fixed_meoh) + b3(100 - ACN - fixed_meoh)
  
  b <- coef(fit)
  
  # แก้สมการหา ACN:
  # target_rt = b[1]*ACN + b[2]*fixed_meoh + b[3]*100 - b[3]*ACN - b[3]*fixed_meoh
  # target_rt - b[2]*fixed_meoh - b[3]*100 + b[3]*fixed_meoh = ACN * (b[1] - b[3])
  
  num <- target_rt - (b[2] * fixed_meoh) - (b[3] * 100) + (b[3] * fixed_meoh)
  den <- b[1] - b[3]
  
  acn_needed <- num / den
  buffer_needed <- 100 - acn_needed - fixed_meoh
  
  return(c(ACN = acn_needed, MeOH = fixed_meoh, Buffer = buffer_needed))
}

# 3. ทดสอบหาค่าสำหรับ RT = 15 นาที โดยล็อค MeOH ไว้ที่ 19%
result <- find_mobile_phase(target_rt = 15, fixed_meoh = 19)
print(result)

library(ggtern)

# 1. เตรียมข้อมูล
df <- data.frame(
  ACN = c(13, 11, 15, 13),
  MeOH = c(19, 19, 19, 17),
  Buffer = c(68, 70, 66, 70),
  RT = c(17.444, 22.252, 13.283, 18.804)
)

# 2. วาดกราฟแบบ Smooth Gradient
ggtern(data = df, aes(x = ACN, y = MeOH, z = Buffer)) +
  # ปรับ n=1000 เพื่อความเนียนกริบของพื้นผิว
  stat_interpolate_tern(aes(value = RT, fill = ..level..), 
                        geom = "polygon", 
                        method = "lm", 
                        n = 1000,        
                        breaks = seq(12, 23, by = 0.5)) + # กำหนดขั้นบันไดสีให้ละเอียดขึ้น
  
  # เพิ่มเส้น Contour เพื่อความชัดเจน
  geom_interpolate_tern(aes(value = RT), 
                        color = "white", 
                        size = 0.2, 
                        alpha = 0.5,
                        method = "lm", 
                        n = 1000) +
  
  geom_point(size = 3, color = "black", alpha = 0.7) +
  
  # ใช้สีแบบ Viridis ที่ดูเป็นวิทยาศาสตร์และไล่ระดับได้เรียบเนียนกว่า
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  
  labs(title = "Optimized Surface for Retention Time",
       subtitle = "Mobile phase ratio",
       fill = "RT (min)") +
  
  theme_rgbw() +
  # Zoom เข้าไปยังพื้นที่ที่มีจุดทดลอง (Adjust ตามความเหมาะสม)
  limit_tern(T = 0.25, L = 0.20, R = 0.75) +
  theme(tern.axis.arrow.show = TRUE) # ใส่ลูกศรทิศทางเพื่อให้ดูง่ายขึ้น


