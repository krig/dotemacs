import os
import os.path

def view_source(filename):
    os.system("emacsclient -n " + filename + " &")
    #os.system("gvim --remote-tab " + filename + " &")

file_types = {
    ".cpp" : view_source, 
    ".c" : view_source, 
    ".h" : view_source, 
    ".py" : view_source,
    ".rb" : view_source,
    ".glade" : lambda (fname): os.system("glade-2 " + fname + " &"),
    ".lua" : view_source,
    ".el" : view_source
    }

SAVE_NAME = os.path.expanduser("~/.jules_folder_list")

