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

        imgnumber = 4

        img = tk.PhotoImage(file=f"/Users/kjy/Desktop/testing/images/{imgnumber}.png")

        imglabel = tk.Label(window, image=img)
        imglabel.pack(side="left")

        comment=tk.StringVar()

        result = tk.Label(font=explain,textvariable=comment)
        result.pack(pady=100)

        comment.set("문코문코")



        window.mainloop()


MainApp = NewTkinter()
MainApp_2 = MainApp.Newwinodw()

