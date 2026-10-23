
/********************* Đăng nhâp tài khoản github trên CMD ********/

git config --global user.name "[Tên TK Github]"
git config --global user.email "[Email đăng ký Github]"

/******************************************************************/

!!! Gửi cho nhóm trưởng Username github để t add vào contributors thì mới push lên được nhé.

/***************** Cách lấy folder và chỉnh sửa *******************/

B1: mở CMD, vào folder cần lữu trữ.

B2:
git clone https://github.com/anhnguyenms2510421-design/BTL-XSTK
cd BTL-XSTK
git switch -c [tên tự đặt 1]
git branch
code .

B3: 
chỉnh sửa phần làm của mình.
Lưu ý: chỉ chỉnh sửa phần Chapter, Image, Package, Source_code của chapter tương ứng, không chỉnh sửa code trong main

B4: 
git status
git add .
git status
git commit -m "[tên tự đặt 2]"

quan sát sự thay đổi trước khi push code

B5:
git push -u origin [tên tự đặt 1]

Lưu ý phần [tên tự đặt 1] phải trùng với tên ở phần "git switch -c" ở B2 nhé.
/******************************************************************/

/********************************* Ví dụ **************************/

Lấy folder:

git clone https://github.com/anhnguyenms2510421-design/BTL-XSTK
cd BTL-XSTK
git switch -c thanh_vien_1
git branch
code .

* Chỉnh sửa code *

Push lên github

git status
git add .
git status
git commit -m "Update Chapter 1"
git push -u origin thanh_vien_1

/******************************************************************/
