import tkinter as tk
import tkinter.font as tkfont

class NewTkinter():
    def Newwinodw(self):
        window = tk.Tk()
        window.geometry("1000x800+200+180")
        window.resizable(True, True)
        window.title("Analyze Result")

        title = tkfont.Font(family="맑은 고딕", size=40, weight="bold")
        explain = tkfont.Font(family="맑은 고딕", size=20)
        buttonfont = tkfont.Font(family="맑은 고딕", )
        textfont = tkfont.Font(family="맑은 고딕", size=17, weight="bold")

        imgnumber = 6

        img = tk.PhotoImage(file=f"{imgnumber}.png")

        imglabel = tk.Label(window, image=img)
        imglabel.pack(side="left")

        comment=tk.StringVar()

        result = tk.Label(font=explain,textvariable=comment)
        result.pack(pady=100)

        comment.set("문재앙은 씹새끼이나, 우리 집값을 올려줬기에 칭찬합니다.")



        window.mainloop()


MainApp = NewTkinter()
MainApp_2 = MainApp.Newwinodw()
