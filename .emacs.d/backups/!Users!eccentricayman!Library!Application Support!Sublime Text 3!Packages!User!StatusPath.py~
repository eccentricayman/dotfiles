import sublime, sublime_plugin, os

class StatusPath(sublime_plugin.EventListener):
  
  def on_activated_async(self, view):
    # print('on_activated_async', view.file_name())ß
    # filename = os.path.split(view.file_name())[1]
    filename = view.file_name()

    if filename is None:
      view.erase_status('_filename')
    else:
      for folder in view.window().folders():
        filename = filename.replace(folder, '.')
      if 'HOME' in os.environ:
        filename = filename.replace(os.environ['HOME'], '~', 1)
      view.set_status('_filename', filename)
