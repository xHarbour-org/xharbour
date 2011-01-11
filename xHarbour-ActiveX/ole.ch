#ifndef _H_OLE
   #define _H_OLE
   
   // Specifies the action that occurs when an end user double-clicks the object in its container. The object, not the container, determines this action. If the object supports in-place activation, the primary verb usually activates the object in place. 
   #define OLEIVERB_PRIMARY 0

   // Instructs an object to show itself for editing or viewing. Called to display newly inserted objects for initial editing and to show link sources. Usually an alias for some other object-defined verb. 
   #define OLEIVERB_SHOW (-1)

   // Instructs an object, including one that otherwise supports in-place activation, to open itself for editing in a window separate from that of its container. If the object does not support in-place activation, this verb has the same semantics as OLEIVERB_SHOW.
   #define OLEIVERB_OPEN (-2)

   // Causes an object to remove its user interface from the view. Applies only to objects that are activated in-place.
   #define OLEIVERB_HIDE (-3)

   // Activates an object in place, along with its full set of user-interface tools, including menus, toolbars, and its name in the title bar of the container window. If the object does not support in-place activation, it should return E_NOTIMPL.
   #define OLEIVERB_UIACTIVATE (-4)

   // Activates an object in place without displaying tools, such as menus and toolbars, that end users need to change the behavior or appearance of the object. Single-clicking such an object causes it to negotiate the display of its user-interface tools with its container. If the container refuses, the object remains active but without its tools displayed.
   #define OLEIVERB_INPLACEACTIVATE (-5)

   #define OLEIVERB_DISCARDUNDOSTATE (-6) 

   #define OLECLOSE_SAVEIFDIRTY  0
   #define OLECLOSE_NOSAVE       1
   #define OLECLOSE_PROMPTSAVE   2
#endif
