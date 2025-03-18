/* See LICENSE file for copyright and license details.
 *
 * dynamic window manager is designed like any other X client as well. It is
 * driven through handling X events. In contrast to other X clients, a window
 * manager selects for SubstructureRedirectMask on the root window, to receive
 * events about window (dis-)appearance. Only one X connection at a time is
 * allowed to select for this event mask.
 *
 * The event handlers of dwm are organized in an array which is accessed
 * whenever a new event has been fetched. This allows event dispatching
 * in O(1) time.
 *
 * Each child of the root window is called a client, except windows which have
 * set the override_redirect flag. Clients are organized in a linked client
 * list on each monitor, the focus history is remembered through a stack list
 * on each monitor. Each client contains a bit array to indicate the tags of a
 * client.
 *
 * Keys and tagging rules are organized as arrays and defined in config.h.
 *
 * To understand everything else, start reading main().
 */
#include <errno.h>
#include <locale.h>
#include <libguile.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Xutil.h>
#ifdef XINERAMA
#include <X11/extensions/Xinerama.h>
#endif /* XINERAMA */
#include <X11/Xft/Xft.h>

#include "drw.h"
#include "util.h"

/* macros */
#define BUTTONMASK              (ButtonPressMask|ButtonReleaseMask)
static unsigned int numlockmask = 0;
#define CLEANMASK(mask)         (mask & ~(numlockmask|LockMask) & (ShiftMask|ControlMask|Mod1Mask|Mod2Mask|Mod3Mask|Mod4Mask|Mod5Mask))
#define INTERSECT(x,y,w,h,m)    (MAX(0, MIN((x)+(w),(m)->wx+(m)->ww) - MAX((x),(m)->wx)) \
                               * MAX(0, MIN((y)+(h),(m)->wy+(m)->wh) - MAX((y),(m)->wy)))
#define ISVISIBLE(C)            ((C->tags & C->mon->tagset[C->mon->seltags]))
#define MOUSEMASK               (BUTTONMASK|PointerMotionMask)
#define WIDTH(X)                ((X)->w + 2 * (X)->bw)
#define HEIGHT(X)               ((X)->h + 2 * (X)->bw)
#define TAGMASK                 ((1 << LENGTH(tags)) - 1)
#define TEXTW(X)                (drw_fontset_getwidth(drw, (X)) + lrpad)

/* enums */
enum { CurNormal, CurResize, CurMove, CurLast }; /* cursor */
enum { SchemeNorm, SchemeSel }; /* color schemes */
enum { NetSupported, NetWMName, NetWMState, NetWMCheck,
       NetWMFullscreen, NetActiveWindow, NetWMWindowType,
       NetWMWindowTypeDialog, NetClientList, NetLast }; /* EWMH atoms */
enum { WMProtocols, WMDelete, WMState, WMTakeFocus, WMLast }; /* default atoms */
enum { ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
       ClkClientWin, ClkRootWin, ClkLast }; /* clicks */

typedef union {
	int i;
	unsigned int ui;
	float f;
	const void *v;
} Arg;

typedef struct {
	unsigned int click;
	unsigned int mask;
	unsigned int button;
	void (*func)(const Arg *arg);
	const Arg arg;
} Button;

typedef struct Monitor Monitor;
typedef struct Client Client;
struct Client {
	char name[256];
	float mina, maxa;
	int x, y, w, h;
	int oldx, oldy, oldw, oldh;
	int basew, baseh, incw, inch, maxw, maxh, minw, minh, hintsvalid;
	int bw, oldbw;
	unsigned int tags;
	int isfixed, isfloating, isurgent, neverfocus, oldstate, isfullscreen;
	Client *next;
	Client *snext;
	Monitor *mon;
	Window win;
};

typedef struct Key Key;
struct Key {
	unsigned int mod;
	KeySym keysym;

  // 0-arg SCM function to be invoked when key hit
  SCM func;

  Key* next;
};

static Key* keys = NULL;

void
add_key(unsigned int mod, KeySym keysym, SCM func) {
  Key* key = ecalloc(1, sizeof(Key));
  key->mod = mod;
  key->keysym = keysym;
  key->func = func;

  if (keys == NULL) {
    keys = key;
  } else {
    key->next = keys;
    keys = key;
    /* Key* curr = keys; */
    /* while (curr->next != NULL) curr = curr->next; */
    /* curr->next = key; */
  }
}

SCM
scm_add_key(SCM scm_mod, SCM scm_keysym, SCM func) {
  uint mod = scm_to_uint(scm_mod);
  KeySym keysym = scm_to_uint(scm_keysym);
  add_key(mod, keysym, func);
  return SCM_BOOL_T;
}

Key* find_key(unsigned int mod, KeySym keysym) {
  Key* curr = keys;
  printf("Searching for key %d %d\n", mod, keysym);
  while(curr != NULL) {
    printf("Checking key: %d %d\n", curr->mod, curr->keysym);
    if (CLEANMASK(curr->mod) == CLEANMASK(mod) && curr->keysym == keysym) {
      printf("Found key: %d %d\n", curr->mod, curr->keysym);
      return curr;
    }
    curr = curr->next;
  }

  return NULL;
}

typedef struct Layout Layout;
struct Layout {
	const char *symbol;

  // must be a function that takes scm_monitor as argument
  SCM arrange;

  Layout *next;
};

Layout *layouts = NULL;
void add_layout(const char *symbol, SCM arrange) {
  Layout *l = malloc(sizeof(Layout));
  l->symbol = symbol;
  l->arrange = arrange;
  l->next = NULL;

  if (layouts == NULL) {
    layouts = l;
  } else {
    l->next = layouts;
    layouts = l;
    /* Layout *curr = layouts; */
    /* while (curr->next != NULL) curr = curr->next; */
    /* curr->next = l; */
  }
}

static SCM
scm_add_layout(SCM scm_symbol, SCM scm_func) {
  char *symbol = scm_to_locale_string(scm_symbol);
  add_layout(symbol, scm_func);
  return SCM_BOOL_T;
}

void
print_layouts() {
  Layout* l = layouts;
  printf("Layouts:\n");
  while (l != NULL) {
    printf("  %s\n", l->symbol);
    l = l->next;
  }
}

Layout* find_layout(const char *symbol) {
  printf("Finding layout %s\n", symbol);
  fflush(stdout);
  Layout *current = layouts;
  while (current != NULL) {
    if (strcmp(current->symbol, symbol) == 0) {
      printf("Found layout %s\n", symbol);
      fflush(stdout);
      return current;
    }
    current = current->next;
  }
  printf("Couldn't find layout %s\n", symbol);
  fflush(stdout);
  return NULL;
}

struct Monitor {
	char ltsymbol[16];
	float mfact;
	int nmaster;
	int num;
	int by;               /* bar geometry */
	int mx, my, mw, mh;   /* screen size */
	int wx, wy, ww, wh;   /* window area  */
	unsigned int seltags;
	unsigned int sellt;
	unsigned int tagset[2];
	int showbar;
	int topbar;
	Client *clients;
	Client *sel;
	Client *stack;
	Monitor *next;
	Window barwin;
	const Layout *lt[2];
};

static SCM monitor_type;
void
scm_define_monitor() {
  SCM name = scm_from_utf8_symbol("Monitor");
  SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
  monitor_type = scm_make_foreign_object_type(name, slots, NULL);
}

SCM
scm_from_monitor(Monitor* m) {
  printf("Wrapping monitor %p", m);
  fflush(stdout);

  SCM monitor = scm_make_foreign_object_1(monitor_type, m);

  printf("Wrapped monitor %p", m);
  fflush(stdout);
  return monitor;
}

Monitor*
scm_to_monitor(SCM m) {
  scm_assert_foreign_object_type(monitor_type, m);
  Monitor* monitor = scm_foreign_object_ref(m, 0);
  return monitor;
}

typedef struct {
	const char *class;
	const char *instance;
	const char *title;
	unsigned int tags;
	int isfloating;
	int monitor;
} Rule;

/* function declarations */
static void applyrules(Client *c);
static int applysizehints(Client *c, int *x, int *y, int *w, int *h, int interact);
static void arrange(Monitor *m);
static void arrangemon(Monitor *m);
static void attach(Client *c);
static void attachstack(Client *c);
static void buttonpress(XEvent *e);
static void checkotherwm(void);
static void cleanup(void);
static void cleanupmon(Monitor *mon);
static void clientmessage(XEvent *e);
static void configure(Client *c);
static void configurenotify(XEvent *e);
static void configurerequest(XEvent *e);
static Monitor *createmon(void);
static void destroynotify(XEvent *e);
static void detach(Client *c);
static void detachstack(Client *c);
static Monitor *dirtomon(int dir);
static void drawbar(Monitor *m);
static void drawbars(void);
static void enternotify(XEvent *e);
static void expose(XEvent *e);
static void focus(Client *c);
static void focusin(XEvent *e);
static void focusmon(const Arg *arg);
static void focusstack(const Arg *arg);
static Atom getatomprop(Client *c, Atom prop);
static int getrootptr(int *x, int *y);
static long getstate(Window w);
static int gettextprop(Window w, Atom atom, char *text, unsigned int size);
static void grabbuttons(Client *c, int focused);
static void grabkeys(void);
static void incnmaster(const Arg *arg);
static void keypress(XEvent *e);
static void killclient(const Arg *arg);
static void manage(Window w, XWindowAttributes *wa);
static void mappingnotify(XEvent *e);
static void maprequest(XEvent *e);
static void monocle(Monitor *m);
static void motionnotify(XEvent *e);
static void movemouse(const Arg *arg);
static Client *nexttiled(Client *c);
static void pop(Client *c);
static void propertynotify(XEvent *e);
static void quit(const Arg *arg);
static Monitor *recttomon(int x, int y, int w, int h);
static void resize(Client *c, int x, int y, int w, int h, int interact);
static void resizeclient(Client *c, int x, int y, int w, int h);
static void resizemouse(const Arg *arg);
static void restack(Monitor *m);
static void run(void);
static void scan(void);
static int sendevent(Client *c, Atom proto);
static void sendmon(Client *c, Monitor *m);
static void setclientstate(Client *c, long state);
static void setfocus(Client *c);
static void setfullscreen(Client *c, int fullscreen);
static void setlayout(const Arg *arg);
static void setlayout_byname(const Arg *arg);
static void setmfact(const Arg *arg);
static void setup(void);
static void seturgent(Client *c, int urg);
static void showhide(Client *c);
static void spawn(const Arg *arg);
static void tag(const Arg *arg);
static void tagmon(const Arg *arg);
static void tile(Monitor *m);
static void togglebar(const Arg *arg);
static void togglefloating(const Arg *arg);
static void toggletag(const Arg *arg);
static void toggleview(const Arg *arg);
static void unfocus(Client *c, int setfocus);
static void unmanage(Client *c, int destroyed);
static void unmapnotify(XEvent *e);
static void updatebarpos(Monitor *m);
static void updatebars(void);
static void updateclientlist(void);
static int updategeom(void);
static void updatenumlockmask(void);
static void updatesizehints(Client *c);
static void updatestatus(void);
static void updatetitle(Client *c);
static void updatewindowtype(Client *c);
static void updatewmhints(Client *c);
static void view(const Arg *arg);
static Client *wintoclient(Window w);
static Monitor *wintomon(Window w);
static int xerror(Display *dpy, XErrorEvent *ee);
static int xerrordummy(Display *dpy, XErrorEvent *ee);
static int xerrorstart(Display *dpy, XErrorEvent *ee);
static void zoom(const Arg *arg);

SCM scm_tile(SCM monitor);
SCM scm_monocle(SCM monitor);
SCM scm_togglebar();
SCM scm_focusstack(SCM num);
SCM scm_incnmaster(SCM num);
SCM scm_setmfact(SCM amount);
SCM scm_zoom();
SCM scm_view(SCM num);
SCM scm_killclient();
SCM scm_setlayout(SCM scm_name);
SCM scm_togglefloating();
SCM scm_tag(SCM num);
SCM scm_focusmon(SCM num);
SCM scm_tagmon(SCM num);
SCM scm_quit();
SCM scm_toggleview(SCM num);
SCM scm_toggletag(SCM num);

    /* variables */
    static const char broken[] = "broken";
static char stext[256];
static int screen;
static int sw, sh;           /* X display screen geometry width, height */
static int bh;               /* bar height */
static int lrpad;            /* sum of left and right padding for text */
static int (*xerrorxlib)(Display *, XErrorEvent *);
static void (*handler[LASTEvent]) (XEvent *) = {
	[ButtonPress] = buttonpress,
	[ClientMessage] = clientmessage,
	[ConfigureRequest] = configurerequest,
	[ConfigureNotify] = configurenotify,
	[DestroyNotify] = destroynotify,
	[EnterNotify] = enternotify,
	[Expose] = expose,
	[FocusIn] = focusin,
	[KeyPress] = keypress,
	[MappingNotify] = mappingnotify,
	[MapRequest] = maprequest,
	[MotionNotify] = motionnotify,
	[PropertyNotify] = propertynotify,
	[UnmapNotify] = unmapnotify
};
static Atom wmatom[WMLast], netatom[NetLast];
static int running = 1;
static Cur *cursor[CurLast];
static Clr **scheme;
static Display *dpy;
static Drw *drw;
static Monitor *mons, *selmon;
static Window root, wmcheckwin;

/* configuration, allows nested code to access above variables */
#include "config.h"

/* compile-time check if all tags fit into an unsigned int bit array. */
struct NumTags { char limitexceeded[LENGTH(tags) > 31 ? -1 : 1]; };

/* function implementations */
void
applyrules(Client *c)
{
	const char *class, *instance;
	unsigned int i;
	const Rule *r;
	Monitor *m;
	XClassHint ch = { NULL, NULL };

	/* rule matching */
	c->isfloating = 0;
	c->tags = 0;
	XGetClassHint(dpy, c->win, &ch);
	class    = ch.res_class ? ch.res_class : broken;
	instance = ch.res_name  ? ch.res_name  : broken;

	for (i = 0; i < LENGTH(rules); i++) {
		r = &rules[i];
		if ((!r->title || strstr(c->name, r->title))
		&& (!r->class || strstr(class, r->class))
		&& (!r->instance || strstr(instance, r->instance)))
		{
			c->isfloating = r->isfloating;
			c->tags |= r->tags;
			for (m = mons; m && m->num != r->monitor; m = m->next);
			if (m)
				c->mon = m;
		}
	}
	if (ch.res_class)
		XFree(ch.res_class);
	if (ch.res_name)
		XFree(ch.res_name);
	c->tags = c->tags & TAGMASK ? c->tags & TAGMASK : c->mon->tagset[c->mon->seltags];
}

int
applysizehints(Client *c, int *x, int *y, int *w, int *h, int interact)
{
	int baseismin;
	Monitor *m = c->mon;

	/* set minimum possible */
	*w = MAX(1, *w);
	*h = MAX(1, *h);
	if (interact) {
		if (*x > sw)
			*x = sw - WIDTH(c);
		if (*y > sh)
			*y = sh - HEIGHT(c);
		if (*x + *w + 2 * c->bw < 0)
			*x = 0;
		if (*y + *h + 2 * c->bw < 0)
			*y = 0;
	} else {
		if (*x >= m->wx + m->ww)
			*x = m->wx + m->ww - WIDTH(c);
		if (*y >= m->wy + m->wh)
			*y = m->wy + m->wh - HEIGHT(c);
		if (*x + *w + 2 * c->bw <= m->wx)
			*x = m->wx;
		if (*y + *h + 2 * c->bw <= m->wy)
			*y = m->wy;
	}
	if (*h < bh)
		*h = bh;
	if (*w < bh)
		*w = bh;
	if (resizehints || c->isfloating || !c->mon->lt[c->mon->sellt]->arrange) {
		if (!c->hintsvalid)
			updatesizehints(c);
		/* see last two sentences in ICCCM 4.1.2.3 */
		baseismin = c->basew == c->minw && c->baseh == c->minh;
		if (!baseismin) { /* temporarily remove base dimensions */
			*w -= c->basew;
			*h -= c->baseh;
		}
		/* adjust for aspect limits */
		if (c->mina > 0 && c->maxa > 0) {
			if (c->maxa < (float)*w / *h)
				*w = *h * c->maxa + 0.5;
			else if (c->mina < (float)*h / *w)
				*h = *w * c->mina + 0.5;
		}
		if (baseismin) { /* increment calculation requires this */
			*w -= c->basew;
			*h -= c->baseh;
		}
		/* adjust for increment value */
		if (c->incw)
			*w -= *w % c->incw;
		if (c->inch)
			*h -= *h % c->inch;
		/* restore base dimensions */
		*w = MAX(*w + c->basew, c->minw);
		*h = MAX(*h + c->baseh, c->minh);
		if (c->maxw)
			*w = MIN(*w, c->maxw);
		if (c->maxh)
			*h = MIN(*h, c->maxh);
	}
	return *x != c->x || *y != c->y || *w != c->w || *h != c->h;
}

void
arrange(Monitor *m)
{
	if (m)
		showhide(m->stack);
	else for (m = mons; m; m = m->next)
		showhide(m->stack);
	if (m) {
		arrangemon(m);
		restack(m);
	} else for (m = mons; m; m = m->next)
		arrangemon(m);
}

SCM
scm_arrange(SCM scm_m) {
  Monitor* m = scm_to_monitor(scm_m);
  arrange(m);
  return SCM_BOOL_T;
}

void
arrangemon(Monitor *m)
{
	strncpy(m->ltsymbol, m->lt[m->sellt]->symbol, sizeof m->ltsymbol);

  SCM arrange = m->lt[m->sellt]->arrange;
  if (scm_is_false(arrange)) return;

  printf("Calling layout %s\n", m->lt[m->sellt]->symbol);
  fflush(stdout);
  scm_call_1(arrange, scm_from_monitor(m));
}

void
attach(Client *c)
{
	c->next = c->mon->clients;
	c->mon->clients = c;
}

void
attachstack(Client *c)
{
	c->snext = c->mon->stack;
	c->mon->stack = c;
}

void
buttonpress(XEvent *e)
{
	unsigned int i, x, click;
	Arg arg = {0};
	Client *c;
	Monitor *m;
	XButtonPressedEvent *ev = &e->xbutton;

	click = ClkRootWin;
	/* focus monitor if necessary */
	if ((m = wintomon(ev->window)) && m != selmon) {
		unfocus(selmon->sel, 1);
		selmon = m;
		focus(NULL);
	}
	if (ev->window == selmon->barwin) {
		i = x = 0;
		do
			x += TEXTW(tags[i]);
		while (ev->x >= x && ++i < LENGTH(tags));
		if (i < LENGTH(tags)) {
			click = ClkTagBar;
			arg.ui = 1 << i;
		} else if (ev->x < x + TEXTW(selmon->ltsymbol))
			click = ClkLtSymbol;
		else if (ev->x > selmon->ww - (int)TEXTW(stext))
			click = ClkStatusText;
		else
			click = ClkWinTitle;
	} else if ((c = wintoclient(ev->window))) {
		focus(c);
		restack(selmon);
		XAllowEvents(dpy, ReplayPointer, CurrentTime);
		click = ClkClientWin;
	}
	for (i = 0; i < LENGTH(buttons); i++)
		if (click == buttons[i].click && buttons[i].func && buttons[i].button == ev->button
		&& CLEANMASK(buttons[i].mask) == CLEANMASK(ev->state))
			buttons[i].func(click == ClkTagBar && buttons[i].arg.i == 0 ? &arg : &buttons[i].arg);
}

void
checkotherwm(void)
{
	xerrorxlib = XSetErrorHandler(xerrorstart);
	/* this causes an error if some other window manager is running */
	XSelectInput(dpy, DefaultRootWindow(dpy), SubstructureRedirectMask);
	XSync(dpy, False);
	XSetErrorHandler(xerror);
	XSync(dpy, False);
}

void
cleanup(void)
{
	Arg a = {.ui = ~0};
	Layout foo = { "", NULL };
	Monitor *m;
	size_t i;

	view(&a);
	selmon->lt[selmon->sellt] = &foo;
	for (m = mons; m; m = m->next)
		while (m->stack)
			unmanage(m->stack, 0);
	XUngrabKey(dpy, AnyKey, AnyModifier, root);
	while (mons)
		cleanupmon(mons);
	for (i = 0; i < CurLast; i++)
		drw_cur_free(drw, cursor[i]);
	for (i = 0; i < LENGTH(colors); i++)
		free(scheme[i]);
	free(scheme);
	XDestroyWindow(dpy, wmcheckwin);
	drw_free(drw);
	XSync(dpy, False);
	XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
	XDeleteProperty(dpy, root, netatom[NetActiveWindow]);
}

void
cleanupmon(Monitor *mon)
{
	Monitor *m;

	if (mon == mons)
		mons = mons->next;
	else {
		for (m = mons; m && m->next != mon; m = m->next);
		m->next = mon->next;
	}
	XUnmapWindow(dpy, mon->barwin);
	XDestroyWindow(dpy, mon->barwin);
	free(mon);
}

void
clientmessage(XEvent *e)
{
	XClientMessageEvent *cme = &e->xclient;
	Client *c = wintoclient(cme->window);

	if (!c)
		return;
	if (cme->message_type == netatom[NetWMState]) {
		if (cme->data.l[1] == netatom[NetWMFullscreen]
		|| cme->data.l[2] == netatom[NetWMFullscreen])
			setfullscreen(c, (cme->data.l[0] == 1 /* _NET_WM_STATE_ADD    */
				|| (cme->data.l[0] == 2 /* _NET_WM_STATE_TOGGLE */ && !c->isfullscreen)));
	} else if (cme->message_type == netatom[NetActiveWindow]) {
		if (c != selmon->sel && !c->isurgent)
			seturgent(c, 1);
	}
}

void
configure(Client *c)
{
	XConfigureEvent ce;

	ce.type = ConfigureNotify;
	ce.display = dpy;
	ce.event = c->win;
	ce.window = c->win;
	ce.x = c->x;
	ce.y = c->y;
	ce.width = c->w;
	ce.height = c->h;
	ce.border_width = c->bw;
	ce.above = None;
	ce.override_redirect = False;
	XSendEvent(dpy, c->win, False, StructureNotifyMask, (XEvent *)&ce);
}

void
configurenotify(XEvent *e)
{
	Monitor *m;
	Client *c;
	XConfigureEvent *ev = &e->xconfigure;
	int dirty;

	/* TODO: updategeom handling sucks, needs to be simplified */
	if (ev->window == root) {
		dirty = (sw != ev->width || sh != ev->height);
		sw = ev->width;
		sh = ev->height;
		if (updategeom() || dirty) {
			drw_resize(drw, sw, bh);
			updatebars();
			for (m = mons; m; m = m->next) {
				for (c = m->clients; c; c = c->next)
					if (c->isfullscreen)
						resizeclient(c, m->mx, m->my, m->mw, m->mh);
				XMoveResizeWindow(dpy, m->barwin, m->wx, m->by, m->ww, bh);
			}
			focus(NULL);
			arrange(NULL);
		}
	}
}

void
configurerequest(XEvent *e)
{
	Client *c;
	Monitor *m;
	XConfigureRequestEvent *ev = &e->xconfigurerequest;
	XWindowChanges wc;

	if ((c = wintoclient(ev->window))) {
		if (ev->value_mask & CWBorderWidth)
			c->bw = ev->border_width;
		else if (c->isfloating || !selmon->lt[selmon->sellt]->arrange) {
			m = c->mon;
			if (ev->value_mask & CWX) {
				c->oldx = c->x;
				c->x = m->mx + ev->x;
			}
			if (ev->value_mask & CWY) {
				c->oldy = c->y;
				c->y = m->my + ev->y;
			}
			if (ev->value_mask & CWWidth) {
				c->oldw = c->w;
				c->w = ev->width;
			}
			if (ev->value_mask & CWHeight) {
				c->oldh = c->h;
				c->h = ev->height;
			}
			if ((c->x + c->w) > m->mx + m->mw && c->isfloating)
				c->x = m->mx + (m->mw / 2 - WIDTH(c) / 2); /* center in x direction */
			if ((c->y + c->h) > m->my + m->mh && c->isfloating)
				c->y = m->my + (m->mh / 2 - HEIGHT(c) / 2); /* center in y direction */
			if ((ev->value_mask & (CWX|CWY)) && !(ev->value_mask & (CWWidth|CWHeight)))
				configure(c);
			if (ISVISIBLE(c))
				XMoveResizeWindow(dpy, c->win, c->x, c->y, c->w, c->h);
		} else
			configure(c);
	} else {
		wc.x = ev->x;
		wc.y = ev->y;
		wc.width = ev->width;
		wc.height = ev->height;
		wc.border_width = ev->border_width;
		wc.sibling = ev->above;
		wc.stack_mode = ev->detail;
		XConfigureWindow(dpy, ev->window, ev->value_mask, &wc);
	}
	XSync(dpy, False);
}

Monitor *
createmon(void)
{
	Monitor *m;
  printf("Creating monitor\n");
  fflush(stdout);

  m = ecalloc(1, sizeof(Monitor));
  m->tagset[0] = m->tagset[1] = 1;
  m->mfact = mfact;
  m->nmaster = nmaster;
  m->showbar = showbar;
  m->topbar = topbar;

  if (layouts == NULL)
    die("No layouts defined!");
	m->lt[0] = layouts;
	m->lt[1] = layouts->next == NULL ? layouts : layouts->next;
	strncpy(m->ltsymbol, layouts->symbol, sizeof m->ltsymbol);
	return m;
}

void
destroynotify(XEvent *e)
{
	Client *c;
	XDestroyWindowEvent *ev = &e->xdestroywindow;

	if ((c = wintoclient(ev->window)))
		unmanage(c, 1);
}

void
detach(Client *c)
{
	Client **tc;

	for (tc = &c->mon->clients; *tc && *tc != c; tc = &(*tc)->next);
	*tc = c->next;
}

void
detachstack(Client *c)
{
	Client **tc, *t;

	for (tc = &c->mon->stack; *tc && *tc != c; tc = &(*tc)->snext);
	*tc = c->snext;

	if (c == c->mon->sel) {
		for (t = c->mon->stack; t && !ISVISIBLE(t); t = t->snext);
		c->mon->sel = t;
	}
}

Monitor *
dirtomon(int dir)
{
	Monitor *m = NULL;

	if (dir > 0) {
		if (!(m = selmon->next))
			m = mons;
	} else if (selmon == mons)
		for (m = mons; m->next; m = m->next);
	else
		for (m = mons; m->next != selmon; m = m->next);
	return m;
}

void
drawbar(Monitor *m)
{
	int x, w, tw = 0;
	int boxs = drw->fonts->h / 9;
	int boxw = drw->fonts->h / 6 + 2;
	unsigned int i, occ = 0, urg = 0;
	Client *c;

	if (!m->showbar)
		return;

	/* draw status first so it can be overdrawn by tags later */
	if (m == selmon) { /* status is only drawn on selected monitor */
		drw_setscheme(drw, scheme[SchemeNorm]);
		tw = TEXTW(stext) - lrpad + 2; /* 2px right padding */
		drw_text(drw, m->ww - tw, 0, tw, bh, 0, stext, 0);
	}

	for (c = m->clients; c; c = c->next) {
		occ |= c->tags;
		if (c->isurgent)
			urg |= c->tags;
	}
	x = 0;
	for (i = 0; i < LENGTH(tags); i++) {
		w = TEXTW(tags[i]);
		drw_setscheme(drw, scheme[m->tagset[m->seltags] & 1 << i ? SchemeSel : SchemeNorm]);
		drw_text(drw, x, 0, w, bh, lrpad / 2, tags[i], urg & 1 << i);
		if (occ & 1 << i)
			drw_rect(drw, x + boxs, boxs, boxw, boxw,
				m == selmon && selmon->sel && selmon->sel->tags & 1 << i,
				urg & 1 << i);
		x += w;
	}
	w = TEXTW(m->ltsymbol);
	drw_setscheme(drw, scheme[SchemeNorm]);
	x = drw_text(drw, x, 0, w, bh, lrpad / 2, m->ltsymbol, 0);

	if ((w = m->ww - tw - x) > bh) {
		if (m->sel) {
			drw_setscheme(drw, scheme[m == selmon ? SchemeSel : SchemeNorm]);
			drw_text(drw, x, 0, w, bh, lrpad / 2, m->sel->name, 0);
			if (m->sel->isfloating)
				drw_rect(drw, x + boxs, boxs, boxw, boxw, m->sel->isfixed, 0);
		} else {
			drw_setscheme(drw, scheme[SchemeNorm]);
			drw_rect(drw, x, 0, w, bh, 1, 1);
		}
	}
	drw_map(drw, m->barwin, 0, 0, m->ww, bh);
}

void
drawbars(void)
{
	Monitor *m;

	for (m = mons; m; m = m->next)
		drawbar(m);
}

void
enternotify(XEvent *e)
{
	Client *c;
	Monitor *m;
	XCrossingEvent *ev = &e->xcrossing;

	if ((ev->mode != NotifyNormal || ev->detail == NotifyInferior) && ev->window != root)
		return;
	c = wintoclient(ev->window);
	m = c ? c->mon : wintomon(ev->window);
	if (m != selmon) {
		unfocus(selmon->sel, 1);
		selmon = m;
	} else if (!c || c == selmon->sel)
		return;
	focus(c);
}

void
expose(XEvent *e)
{
	Monitor *m;
	XExposeEvent *ev = &e->xexpose;

	if (ev->count == 0 && (m = wintomon(ev->window)))
		drawbar(m);
}

void
focus(Client *c)
{
	if (!c || !ISVISIBLE(c))
		for (c = selmon->stack; c && !ISVISIBLE(c); c = c->snext);
	if (selmon->sel && selmon->sel != c)
		unfocus(selmon->sel, 0);
	if (c) {
		if (c->mon != selmon)
			selmon = c->mon;
		if (c->isurgent)
			seturgent(c, 0);
		detachstack(c);
		attachstack(c);
		grabbuttons(c, 1);
		XSetWindowBorder(dpy, c->win, scheme[SchemeSel][ColBorder].pixel);
		setfocus(c);
	} else {
		XSetInputFocus(dpy, root, RevertToPointerRoot, CurrentTime);
		XDeleteProperty(dpy, root, netatom[NetActiveWindow]);
	}
	selmon->sel = c;
	drawbars();
}

/* there are some broken focus acquiring clients needing extra handling */
void
focusin(XEvent *e)
{
	XFocusChangeEvent *ev = &e->xfocus;

	if (selmon->sel && ev->window != selmon->sel->win)
		setfocus(selmon->sel);
}

void
focusmon(const Arg *arg)
{
	Monitor *m;

	if (!mons->next)
		return;
	if ((m = dirtomon(arg->i)) == selmon)
		return;
	unfocus(selmon->sel, 0);
	selmon = m;
	focus(NULL);
}

SCM
scm_focusmon(SCM num) {
  const Arg arg = {.i = scm_to_int(num)};
  focusmon(&arg);
  return SCM_BOOL_T;
}

void
focusstack(const Arg *arg)
{
	Client *c = NULL, *i;

	if (!selmon->sel || (selmon->sel->isfullscreen && lockfullscreen))
		return;
	if (arg->i > 0) {
		for (c = selmon->sel->next; c && !ISVISIBLE(c); c = c->next);
		if (!c)
			for (c = selmon->clients; c && !ISVISIBLE(c); c = c->next);
	} else {
		for (i = selmon->clients; i != selmon->sel; i = i->next)
			if (ISVISIBLE(i))
				c = i;
		if (!c)
			for (; i; i = i->next)
				if (ISVISIBLE(i))
					c = i;
	}
	if (c) {
		focus(c);
		restack(selmon);
	}
}

SCM
scm_focusstack(SCM num) {
  const Arg arg = {.i = scm_to_int(num)};
  focusstack(&arg);
  return SCM_BOOL_T;
}

Atom
getatomprop(Client *c, Atom prop)
{
	int di;
	unsigned long dl;
	unsigned char *p = NULL;
	Atom da, atom = None;

	if (XGetWindowProperty(dpy, c->win, prop, 0L, sizeof atom, False, XA_ATOM,
		&da, &di, &dl, &dl, &p) == Success && p) {
		atom = *(Atom *)p;
		XFree(p);
	}
	return atom;
}

int
getrootptr(int *x, int *y)
{
	int di;
	unsigned int dui;
	Window dummy;

	return XQueryPointer(dpy, root, &dummy, &dummy, x, y, &di, &di, &dui);
}

long
getstate(Window w)
{
	int format;
	long result = -1;
	unsigned char *p = NULL;
	unsigned long n, extra;
	Atom real;

	if (XGetWindowProperty(dpy, w, wmatom[WMState], 0L, 2L, False, wmatom[WMState],
		&real, &format, &n, &extra, (unsigned char **)&p) != Success)
		return -1;
	if (n != 0)
		result = *p;
	XFree(p);
	return result;
}

int
gettextprop(Window w, Atom atom, char *text, unsigned int size)
{
	char **list = NULL;
	int n;
	XTextProperty name;

	if (!text || size == 0)
		return 0;
	text[0] = '\0';
	if (!XGetTextProperty(dpy, w, &name, atom) || !name.nitems)
		return 0;
	if (name.encoding == XA_STRING) {
		strncpy(text, (char *)name.value, size - 1);
	} else if (XmbTextPropertyToTextList(dpy, &name, &list, &n) >= Success && n > 0 && *list) {
		strncpy(text, *list, size - 1);
		XFreeStringList(list);
	}
	text[size - 1] = '\0';
	XFree(name.value);
	return 1;
}

void
grabbuttons(Client *c, int focused)
{
	updatenumlockmask();
	{
		unsigned int i, j;
		unsigned int modifiers[] = { 0, LockMask, numlockmask, numlockmask|LockMask };
		XUngrabButton(dpy, AnyButton, AnyModifier, c->win);
		if (!focused)
			XGrabButton(dpy, AnyButton, AnyModifier, c->win, False,
				BUTTONMASK, GrabModeSync, GrabModeSync, None, None);
		for (i = 0; i < LENGTH(buttons); i++)
			if (buttons[i].click == ClkClientWin)
				for (j = 0; j < LENGTH(modifiers); j++)
					XGrabButton(dpy, buttons[i].button,
						buttons[i].mask | modifiers[j],
						c->win, False, BUTTONMASK,
						GrabModeAsync, GrabModeSync, None, None);
	}
}

void
grabkeys(void)
{
	updatenumlockmask();
	{
		unsigned int i, j, k;
		unsigned int modifiers[] = { 0, LockMask, numlockmask, numlockmask|LockMask };
		int start, end, skip;
		KeySym *syms;

		XUngrabKey(dpy, AnyKey, AnyModifier, root);
		XDisplayKeycodes(dpy, &start, &end);
		syms = XGetKeyboardMapping(dpy, start, end - start + 1, &skip);
		if (!syms)
			return;
		for (k = start; k <= end; k++) {
      Key* key = keys;
      while (key != NULL) {
        /* skip modifier codes, we do that ourselves */
        if (key->keysym == syms[(k - start) * skip])
          for (j = 0; j < LENGTH(modifiers); j++)
            XGrabKey(dpy, k, key->mod | modifiers[j], root, True,
                     GrabModeAsync, GrabModeAsync);
        key = key->next;
      }
    }

    XFree(syms);
	}
}

void
incnmaster(const Arg *arg)
{
	selmon->nmaster = MAX(selmon->nmaster + arg->i, 0);
	arrange(selmon);
}

SCM
scm_incnmaster(SCM num) {
  const Arg arg = {.i = scm_to_int(num)};
  incnmaster(&arg);
  return SCM_BOOL_T;
}

#ifdef XINERAMA
static int
isuniquegeom(XineramaScreenInfo *unique, size_t n, XineramaScreenInfo *info)
{
	while (n--)
		if (unique[n].x_org == info->x_org && unique[n].y_org == info->y_org
		&& unique[n].width == info->width && unique[n].height == info->height)
			return 0;
	return 1;
}
#endif /* XINERAMA */

void
keypress(XEvent *e)
{
	unsigned int i;
	KeySym keysym;
	XKeyEvent *ev;

  printf("Processing key event\n");

  ev = &e->xkey;
  keysym = XKeycodeToKeysym(dpy, (KeyCode)ev->keycode, 0);
  Key *key = find_key(ev->state, keysym);
  if (key != NULL) {
    printf("Running key %d %d\n", ev->state, keysym);
    scm_call_0(key->func);
  }
}

void
killclient(const Arg *arg)
{
	if (!selmon->sel)
		return;
	if (!sendevent(selmon->sel, wmatom[WMDelete])) {
		XGrabServer(dpy);
		XSetErrorHandler(xerrordummy);
		XSetCloseDownMode(dpy, DestroyAll);
		XKillClient(dpy, selmon->sel->win);
		XSync(dpy, False);
		XSetErrorHandler(xerror);
		XUngrabServer(dpy);
	}
}

SCM
scm_killclient() {
  killclient(NULL);
  return SCM_BOOL_T;
}

void
manage(Window w, XWindowAttributes *wa)
{
	Client *c, *t = NULL;
	Window trans = None;
	XWindowChanges wc;

	c = ecalloc(1, sizeof(Client));
	c->win = w;
	/* geometry */
	c->x = c->oldx = wa->x;
	c->y = c->oldy = wa->y;
	c->w = c->oldw = wa->width;
	c->h = c->oldh = wa->height;
	c->oldbw = wa->border_width;

	updatetitle(c);
	if (XGetTransientForHint(dpy, w, &trans) && (t = wintoclient(trans))) {
		c->mon = t->mon;
		c->tags = t->tags;
	} else {
		c->mon = selmon;
		applyrules(c);
	}

	if (c->x + WIDTH(c) > c->mon->wx + c->mon->ww)
		c->x = c->mon->wx + c->mon->ww - WIDTH(c);
	if (c->y + HEIGHT(c) > c->mon->wy + c->mon->wh)
		c->y = c->mon->wy + c->mon->wh - HEIGHT(c);
	c->x = MAX(c->x, c->mon->wx);
	c->y = MAX(c->y, c->mon->wy);
	c->bw = borderpx;

	wc.border_width = c->bw;
	XConfigureWindow(dpy, w, CWBorderWidth, &wc);
	XSetWindowBorder(dpy, w, scheme[SchemeNorm][ColBorder].pixel);
	configure(c); /* propagates border_width, if size doesn't change */
	updatewindowtype(c);
	updatesizehints(c);
	updatewmhints(c);
	XSelectInput(dpy, w, EnterWindowMask|FocusChangeMask|PropertyChangeMask|StructureNotifyMask);
	grabbuttons(c, 0);
	if (!c->isfloating)
		c->isfloating = c->oldstate = trans != None || c->isfixed;
	if (c->isfloating)
		XRaiseWindow(dpy, c->win);
	attach(c);
	attachstack(c);
	XChangeProperty(dpy, root, netatom[NetClientList], XA_WINDOW, 32, PropModeAppend,
		(unsigned char *) &(c->win), 1);
	XMoveResizeWindow(dpy, c->win, c->x + 2 * sw, c->y, c->w, c->h); /* some windows require this */
	setclientstate(c, NormalState);
	if (c->mon == selmon)
		unfocus(selmon->sel, 0);
	c->mon->sel = c;
	arrange(c->mon);
	XMapWindow(dpy, c->win);
	focus(NULL);
}

void
mappingnotify(XEvent *e)
{
	XMappingEvent *ev = &e->xmapping;

	XRefreshKeyboardMapping(ev);
	if (ev->request == MappingKeyboard)
		grabkeys();
}

void
maprequest(XEvent *e)
{
	static XWindowAttributes wa;
	XMapRequestEvent *ev = &e->xmaprequest;

	if (!XGetWindowAttributes(dpy, ev->window, &wa) || wa.override_redirect)
		return;
	if (!wintoclient(ev->window))
		manage(ev->window, &wa);
}

void
monocle(Monitor *m)
{
	unsigned int n = 0;
	Client *c;

	for (c = m->clients; c; c = c->next)
		if (ISVISIBLE(c))
			n++;
	if (n > 0) /* override layout symbol */
		snprintf(m->ltsymbol, sizeof m->ltsymbol, "[%d]", n);
	for (c = nexttiled(m->clients); c; c = nexttiled(c->next))
		resize(c, m->wx, m->wy, m->ww - 2 * c->bw, m->wh - 2 * c->bw, 0);
}

SCM
scm_monocle(SCM m) {
  Monitor* monitor = scm_to_monitor(m);
  monocle(monitor);
  return SCM_UNSPECIFIED;
}

void
motionnotify(XEvent *e)
{
	static Monitor *mon = NULL;
	Monitor *m;
	XMotionEvent *ev = &e->xmotion;

	if (ev->window != root)
		return;
	if ((m = recttomon(ev->x_root, ev->y_root, 1, 1)) != mon && mon) {
		unfocus(selmon->sel, 1);
		selmon = m;
		focus(NULL);
	}
	mon = m;
}

void
movemouse(const Arg *arg)
{
	int x, y, ocx, ocy, nx, ny;
	Client *c;
	Monitor *m;
	XEvent ev;
	Time lasttime = 0;

	if (!(c = selmon->sel))
		return;
	if (c->isfullscreen) /* no support moving fullscreen windows by mouse */
		return;
	restack(selmon);
	ocx = c->x;
	ocy = c->y;
	if (XGrabPointer(dpy, root, False, MOUSEMASK, GrabModeAsync, GrabModeAsync,
		None, cursor[CurMove]->cursor, CurrentTime) != GrabSuccess)
		return;
	if (!getrootptr(&x, &y))
		return;
	do {
		XMaskEvent(dpy, MOUSEMASK|ExposureMask|SubstructureRedirectMask, &ev);
		switch(ev.type) {
		case ConfigureRequest:
		case Expose:
		case MapRequest:
			handler[ev.type](&ev);
			break;
		case MotionNotify:
			if ((ev.xmotion.time - lasttime) <= (1000 / 60))
				continue;
			lasttime = ev.xmotion.time;

			nx = ocx + (ev.xmotion.x - x);
			ny = ocy + (ev.xmotion.y - y);
			if (abs(selmon->wx - nx) < snap)
				nx = selmon->wx;
			else if (abs((selmon->wx + selmon->ww) - (nx + WIDTH(c))) < snap)
				nx = selmon->wx + selmon->ww - WIDTH(c);
			if (abs(selmon->wy - ny) < snap)
				ny = selmon->wy;
			else if (abs((selmon->wy + selmon->wh) - (ny + HEIGHT(c))) < snap)
				ny = selmon->wy + selmon->wh - HEIGHT(c);
			if (!c->isfloating && selmon->lt[selmon->sellt]->arrange
			&& (abs(nx - c->x) > snap || abs(ny - c->y) > snap))
				togglefloating(NULL);
			if (!selmon->lt[selmon->sellt]->arrange || c->isfloating)
				resize(c, nx, ny, c->w, c->h, 1);
			break;
		}
	} while (ev.type != ButtonRelease);
	XUngrabPointer(dpy, CurrentTime);
	if ((m = recttomon(c->x, c->y, c->w, c->h)) != selmon) {
		sendmon(c, m);
		selmon = m;
		focus(NULL);
	}
}

Client *
nexttiled(Client *c)
{
	for (; c && (c->isfloating || !ISVISIBLE(c)); c = c->next);
	return c;
}

void
pop(Client *c)
{
	detach(c);
	attach(c);
	focus(c);
	arrange(c->mon);
}

void
propertynotify(XEvent *e)
{
	Client *c;
	Window trans;
	XPropertyEvent *ev = &e->xproperty;

	if ((ev->window == root) && (ev->atom == XA_WM_NAME))
		updatestatus();
	else if (ev->state == PropertyDelete)
		return; /* ignore */
	else if ((c = wintoclient(ev->window))) {
		switch(ev->atom) {
		default: break;
		case XA_WM_TRANSIENT_FOR:
			if (!c->isfloating && (XGetTransientForHint(dpy, c->win, &trans)) &&
				(c->isfloating = (wintoclient(trans)) != NULL))
				arrange(c->mon);
			break;
		case XA_WM_NORMAL_HINTS:
			c->hintsvalid = 0;
			break;
		case XA_WM_HINTS:
			updatewmhints(c);
			drawbars();
			break;
		}
		if (ev->atom == XA_WM_NAME || ev->atom == netatom[NetWMName]) {
			updatetitle(c);
			if (c == c->mon->sel)
				drawbar(c->mon);
		}
		if (ev->atom == netatom[NetWMWindowType])
			updatewindowtype(c);
	}
}

void
quit(const Arg *arg)
{
	running = 0;
}

SCM
scm_quit() {
  quit(NULL);
  return SCM_BOOL_T;
}

Monitor *
recttomon(int x, int y, int w, int h)
{
	Monitor *m, *r = selmon;
	int a, area = 0;

	for (m = mons; m; m = m->next)
		if ((a = INTERSECT(x, y, w, h, m)) > area) {
			area = a;
			r = m;
		}
	return r;
}

void
resize(Client *c, int x, int y, int w, int h, int interact)
{
	if (applysizehints(c, &x, &y, &w, &h, interact))
		resizeclient(c, x, y, w, h);
}

void
resizeclient(Client *c, int x, int y, int w, int h)
{
	XWindowChanges wc;

	c->oldx = c->x; c->x = wc.x = x;
	c->oldy = c->y; c->y = wc.y = y;
	c->oldw = c->w; c->w = wc.width = w;
	c->oldh = c->h; c->h = wc.height = h;
	wc.border_width = c->bw;
	XConfigureWindow(dpy, c->win, CWX|CWY|CWWidth|CWHeight|CWBorderWidth, &wc);
	configure(c);
	XSync(dpy, False);
}

void
resizemouse(const Arg *arg)
{
	int ocx, ocy, nw, nh;
	Client *c;
	Monitor *m;
	XEvent ev;
	Time lasttime = 0;

	if (!(c = selmon->sel))
		return;
	if (c->isfullscreen) /* no support resizing fullscreen windows by mouse */
		return;
	restack(selmon);
	ocx = c->x;
	ocy = c->y;
	if (XGrabPointer(dpy, root, False, MOUSEMASK, GrabModeAsync, GrabModeAsync,
		None, cursor[CurResize]->cursor, CurrentTime) != GrabSuccess)
		return;
	XWarpPointer(dpy, None, c->win, 0, 0, 0, 0, c->w + c->bw - 1, c->h + c->bw - 1);
	do {
		XMaskEvent(dpy, MOUSEMASK|ExposureMask|SubstructureRedirectMask, &ev);
		switch(ev.type) {
		case ConfigureRequest:
		case Expose:
		case MapRequest:
			handler[ev.type](&ev);
			break;
		case MotionNotify:
			if ((ev.xmotion.time - lasttime) <= (1000 / 60))
				continue;
			lasttime = ev.xmotion.time;

			nw = MAX(ev.xmotion.x - ocx - 2 * c->bw + 1, 1);
			nh = MAX(ev.xmotion.y - ocy - 2 * c->bw + 1, 1);
			if (c->mon->wx + nw >= selmon->wx && c->mon->wx + nw <= selmon->wx + selmon->ww
			&& c->mon->wy + nh >= selmon->wy && c->mon->wy + nh <= selmon->wy + selmon->wh)
			{
				if (!c->isfloating && selmon->lt[selmon->sellt]->arrange
				&& (abs(nw - c->w) > snap || abs(nh - c->h) > snap))
					togglefloating(NULL);
			}
			if (!selmon->lt[selmon->sellt]->arrange || c->isfloating)
				resize(c, c->x, c->y, nw, nh, 1);
			break;
		}
	} while (ev.type != ButtonRelease);
	XWarpPointer(dpy, None, c->win, 0, 0, 0, 0, c->w + c->bw - 1, c->h + c->bw - 1);
	XUngrabPointer(dpy, CurrentTime);
	while (XCheckMaskEvent(dpy, EnterWindowMask, &ev));
	if ((m = recttomon(c->x, c->y, c->w, c->h)) != selmon) {
		sendmon(c, m);
		selmon = m;
		focus(NULL);
	}
}

void
restack(Monitor *m)
{
	Client *c;
	XEvent ev;
	XWindowChanges wc;

	drawbar(m);
	if (!m->sel)
		return;
	if (m->sel->isfloating || !m->lt[m->sellt]->arrange)
		XRaiseWindow(dpy, m->sel->win);
	if (m->lt[m->sellt]->arrange) {
		wc.stack_mode = Below;
		wc.sibling = m->barwin;
		for (c = m->stack; c; c = c->snext)
			if (!c->isfloating && ISVISIBLE(c)) {
				XConfigureWindow(dpy, c->win, CWSibling|CWStackMode, &wc);
				wc.sibling = c->win;
			}
	}
	XSync(dpy, False);
	while (XCheckMaskEvent(dpy, EnterWindowMask, &ev));
}

void
run(void)
{
	XEvent ev;
	/* main event loop */
	XSync(dpy, False);
	while (running && !XNextEvent(dpy, &ev))
		if (handler[ev.type])
			handler[ev.type](&ev); /* call handler */
}

void
scan(void)
{
	unsigned int i, num;
	Window d1, d2, *wins = NULL;
	XWindowAttributes wa;

	if (XQueryTree(dpy, root, &d1, &d2, &wins, &num)) {
		for (i = 0; i < num; i++) {
			if (!XGetWindowAttributes(dpy, wins[i], &wa)
			|| wa.override_redirect || XGetTransientForHint(dpy, wins[i], &d1))
				continue;
			if (wa.map_state == IsViewable || getstate(wins[i]) == IconicState)
				manage(wins[i], &wa);
		}
		for (i = 0; i < num; i++) { /* now the transients */
			if (!XGetWindowAttributes(dpy, wins[i], &wa))
				continue;
			if (XGetTransientForHint(dpy, wins[i], &d1)
			&& (wa.map_state == IsViewable || getstate(wins[i]) == IconicState))
				manage(wins[i], &wa);
		}
		if (wins)
			XFree(wins);
	}
}

void
sendmon(Client *c, Monitor *m)
{
	if (c->mon == m)
		return;
	unfocus(c, 1);
	detach(c);
	detachstack(c);
	c->mon = m;
	c->tags = m->tagset[m->seltags]; /* assign tags of target monitor */
	attach(c);
	attachstack(c);
	focus(NULL);
	arrange(NULL);
}

void
setclientstate(Client *c, long state)
{
	long data[] = { state, None };

	XChangeProperty(dpy, c->win, wmatom[WMState], wmatom[WMState], 32,
		PropModeReplace, (unsigned char *)data, 2);
}

int
sendevent(Client *c, Atom proto)
{
	int n;
	Atom *protocols;
	int exists = 0;
	XEvent ev;

	if (XGetWMProtocols(dpy, c->win, &protocols, &n)) {
		while (!exists && n--)
			exists = protocols[n] == proto;
		XFree(protocols);
	}
	if (exists) {
		ev.type = ClientMessage;
		ev.xclient.window = c->win;
		ev.xclient.message_type = wmatom[WMProtocols];
		ev.xclient.format = 32;
		ev.xclient.data.l[0] = proto;
		ev.xclient.data.l[1] = CurrentTime;
		XSendEvent(dpy, c->win, False, NoEventMask, &ev);
	}
	return exists;
}

void
setfocus(Client *c)
{
	if (!c->neverfocus) {
		XSetInputFocus(dpy, c->win, RevertToPointerRoot, CurrentTime);
		XChangeProperty(dpy, root, netatom[NetActiveWindow],
			XA_WINDOW, 32, PropModeReplace,
			(unsigned char *) &(c->win), 1);
	}
	sendevent(c, wmatom[WMTakeFocus]);
}

void
setfullscreen(Client *c, int fullscreen)
{
	if (fullscreen && !c->isfullscreen) {
		XChangeProperty(dpy, c->win, netatom[NetWMState], XA_ATOM, 32,
			PropModeReplace, (unsigned char*)&netatom[NetWMFullscreen], 1);
		c->isfullscreen = 1;
		c->oldstate = c->isfloating;
		c->oldbw = c->bw;
		c->bw = 0;
		c->isfloating = 1;
		resizeclient(c, c->mon->mx, c->mon->my, c->mon->mw, c->mon->mh);
		XRaiseWindow(dpy, c->win);
	} else if (!fullscreen && c->isfullscreen){
		XChangeProperty(dpy, c->win, netatom[NetWMState], XA_ATOM, 32,
			PropModeReplace, (unsigned char*)0, 0);
		c->isfullscreen = 0;
		c->isfloating = c->oldstate;
		c->bw = c->oldbw;
		c->x = c->oldx;
		c->y = c->oldy;
		c->w = c->oldw;
		c->h = c->oldh;
		resizeclient(c, c->x, c->y, c->w, c->h);
		arrange(c->mon);
	}
}

void
setlayout(const Arg *arg)
{
	if (!arg || !arg->v || arg->v != selmon->lt[selmon->sellt])
		selmon->sellt ^= 1;
	if (arg && arg->v)
		selmon->lt[selmon->sellt] = (Layout *)arg->v;
	strncpy(selmon->ltsymbol, selmon->lt[selmon->sellt]->symbol, sizeof selmon->ltsymbol);
	if (selmon->sel)
		arrange(selmon);
	else
		drawbar(selmon);
}

SCM
scm_setlayout(SCM scm_name) {
  const char* name = scm_to_locale_string(scm_name);
  const Arg arg = {.v = name};
  setlayout_byname(&arg);
  free(name);
  return SCM_BOOL_T;
}

void
setlayout_byname(const Arg *arg) {
  const char* symbol = arg->v;
  Layout* l = find_layout(symbol);
  if (l == NULL) return;
  Arg a = {.v = l};
  setlayout(&a);
}

/* arg > 1.0 will set mfact absolutely */
void
setmfact(const Arg *arg)
{
	float f;

	if (!arg || !selmon->lt[selmon->sellt]->arrange)
		return;
	f = arg->f < 1.0 ? arg->f + selmon->mfact : arg->f - 1.0;
	if (f < 0.05 || f > 0.95)
		return;
	selmon->mfact = f;
	arrange(selmon);
}

SCM
scm_setmfact(SCM amount) {
  const Arg arg = {.f = scm_to_double(amount)};
  setmfact(&arg);
  return SCM_BOOL_T;
}

uint get_xkey(char c) {
  switch (c) {
  case 'b': return XK_b;
  case 'c': return XK_c;
  case 'd': return XK_d;
  case 'f': return XK_f;
  case 'h': return XK_h;
  case 'i': return XK_i;
  case 'j': return XK_j;
  case 'k': return XK_k;
  case 'l': return XK_l;
  case 'm': return XK_m;
  case 'p': return XK_p;
  case 't': return XK_t;
  case 'q': return XK_q;
  case 'r': return XK_r;

  case '0': return XK_0;
  case '1': return XK_1;
  case '2': return XK_2;
  case '3': return XK_3;
  case '4': return XK_4;
  case '5': return XK_5;
  case '6': return XK_6;
  case '7': return XK_7;
  case '8': return XK_8;
  case '9': return XK_9;

  case '.':
    return XK_period;
  case ',':
    return XK_comma;
  case ' ':
    return XK_space;
  case '\t':
    return XK_Tab;
  case '\n':
    return XK_Return;
  }

  return 0;
}

SCM scm_xkey(SCM scm_c) {
  char c = scm_to_char(scm_char_to_integer(scm_c));
  uint xkey = get_xkey(c);
  return scm_from_uint(xkey);
}

uint get_modmask(uint num) {
  switch (num) {
  case 1:
    return Mod1Mask;
  case 2:
    return Mod2Mask;
  case 3:
    return Mod3Mask;
  case 4:
    return Mod4Mask;
  }

  return 0;
}

SCM scm_get_modmask(SCM num) {
  uint n = scm_to_uint(num);
  return scm_from_uint(get_modmask(n));
}

uint get_shiftmask() { return ShiftMask; }
SCM scm_get_shiftmask() { return scm_from_uint(get_shiftmask()); }
uint get_controlmask() { return ControlMask; }
SCM scm_get_controlmask() { return scm_from_uint(get_controlmask()); }

char** scm_list_to_string_array(SCM list) {
  int total = scm_to_int(scm_length(list));
  char** array = calloc(total, sizeof(char*));
  for (int i = 0; i < total; i++) {
    array[i] = scm_to_locale_string(scm_list_ref(list, scm_from_int(i)));
  }
  return array;
}

void free_scm_string_array(char** scm_array, int length) {
  for (int i = 0; i < length; i++) {
    free(scm_array[i]);
  }

  free(scm_array);
}

SCM
scm_dmenumon() {
  char mon = '0' + selmon->num;
  return scm_from_char(mon);
}

SCM scm_load_config() {
  scm_c_primitive_load("/home/dan/pkg/dwm-scheme/scheme/config.scm");
  return SCM_BOOL_T;
}

void
setup(void)
{
	int i;
	XSetWindowAttributes wa;
	Atom utf8string;
	struct sigaction sa;

	/* do not transform children into zombies when they terminate */
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_NOCLDSTOP | SA_NOCLDWAIT | SA_RESTART;
	sa.sa_handler = SIG_IGN;
	sigaction(SIGCHLD, &sa, NULL);

	/* clean up any zombies (inherited from .xinitrc etc) immediately */
	while (waitpid(-1, NULL, WNOHANG) > 0);

  printf("cleaned zombies...\n");
  fflush(stdout);

  printf("initing guile...\n");
  fflush(stdout);
  scm_init_guile();

  scm_define_monitor();

  scm_c_define_gsubr("toggle-bar", 0, 0, 0, scm_togglebar);
  scm_c_define_gsubr("focus-stack", 1, 0, 0, scm_focusstack);
  scm_c_define_gsubr("increase-num-master", 1, 0, 0, scm_incnmaster);
  scm_c_define_gsubr("adjust-master-factor", 1, 0, 0, scm_setmfact);
  scm_c_define_gsubr("zoom", 0, 0, 0, scm_zoom);
  scm_c_define_gsubr("view", 1, 0, 0, scm_view);
  scm_c_define_gsubr("kill-client", 0, 0, 0, scm_killclient);
  scm_c_define_gsubr("set-layout", 1, 0, 0, scm_setlayout);
  scm_c_define_gsubr("add-layout", 2, 0, 0, scm_add_layout);
  scm_c_define_gsubr("tile-layout", 1, 0, 0, scm_tile);
  scm_c_define_gsubr("monocle-layout", 1, 0, 0, scm_monocle);
  scm_c_define_gsubr("toggle-floating", 0, 0, 0, scm_togglefloating);
  scm_c_define_gsubr("tag", 1, 0, 0, scm_tag);
  scm_c_define_gsubr("focus-monitor", 1, 0, 0, scm_focusmon);
  scm_c_define_gsubr("tag-monitor", 1, 0, 0, scm_tagmon);
  scm_c_define_gsubr("quit", 0, 0, 0, scm_quit);
  scm_c_define_gsubr("toggle-view", 1, 0, 0, scm_toggleview);
  scm_c_define_gsubr("toggle-tag", 1, 0, 0, scm_toggletag);
  scm_c_define_gsubr("get-xkey", 1, 0, 0, scm_xkey);
  scm_c_define_gsubr("get-modmask", 1, 0, 0, scm_get_modmask);
  scm_c_define_gsubr("get-shiftmask", 0, 0, 0, scm_get_shiftmask);
  scm_c_define_gsubr("get-controlmask", 0, 0, 0, scm_get_controlmask);
  scm_c_define_gsubr("add-key", 3, 0, 0, scm_add_key);
  scm_c_define_gsubr("dmenu-monitor", 0, 0, 0, scm_dmenumon);
  scm_c_define_gsubr("load-config", 0, 0, 0, scm_load_config);

  scm_load_config();

  printf("initing screen...\n");
  fflush(stdout);

  /* init screen */
  screen = DefaultScreen(dpy);
  printf("initing root window...\n");
  fflush(stdout);
  sw = DisplayWidth(dpy, screen);
  sh = DisplayHeight(dpy, screen);
  root = RootWindow(dpy, screen);

  printf("initing draw create...\n");
  fflush(stdout);
  drw = drw_create(dpy, screen, root, sw, sh);

  SCM scm_fonts = scm_variable_ref(scm_c_lookup("fonts"));
  int total_fonts = scm_to_int(scm_length(scm_fonts));
  char** fonts = scm_list_to_string_array(scm_fonts);

  if (!drw_fontset_create(drw, (const char**) fonts, total_fonts))
    die("no fonts could be loaded.");
  lrpad = drw->fonts->h;
  bh = drw->fonts->h + 2;
  printf("updating geom...\n");
  fflush(stdout);
  updategeom();

  printf("initing atoms...\n");
  fflush(stdout);
	/* init atoms */
	utf8string = XInternAtom(dpy, "UTF8_STRING", False);
	wmatom[WMProtocols] = XInternAtom(dpy, "WM_PROTOCOLS", False);
	wmatom[WMDelete] = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
	wmatom[WMState] = XInternAtom(dpy, "WM_STATE", False);
	wmatom[WMTakeFocus] = XInternAtom(dpy, "WM_TAKE_FOCUS", False);
	netatom[NetActiveWindow] = XInternAtom(dpy, "_NET_ACTIVE_WINDOW", False);
	netatom[NetSupported] = XInternAtom(dpy, "_NET_SUPPORTED", False);
	netatom[NetWMName] = XInternAtom(dpy, "_NET_WM_NAME", False);
	netatom[NetWMState] = XInternAtom(dpy, "_NET_WM_STATE", False);
	netatom[NetWMCheck] = XInternAtom(dpy, "_NET_SUPPORTING_WM_CHECK", False);
	netatom[NetWMFullscreen] = XInternAtom(dpy, "_NET_WM_STATE_FULLSCREEN", False);
	netatom[NetWMWindowType] = XInternAtom(dpy, "_NET_WM_WINDOW_TYPE", False);
	netatom[NetWMWindowTypeDialog] = XInternAtom(dpy, "_NET_WM_WINDOW_TYPE_DIALOG", False);
        netatom[NetClientList] = XInternAtom(dpy, "_NET_CLIENT_LIST", False);

        printf("initing cursors...\n");
        fflush(stdout);
        /* init cursors */
        cursor[CurNormal] = drw_cur_create(drw, XC_left_ptr);
        cursor[CurResize] = drw_cur_create(drw, XC_sizing);
        cursor[CurMove] = drw_cur_create(drw, XC_fleur);

        printf("initing appearance...\n");
        fflush(stdout);
        /* init appearance */
        scheme = ecalloc(LENGTH(colors), sizeof(Clr *));
        for (i = 0; i < LENGTH(colors); i++)
          scheme[i] = drw_scm_create(drw, colors[i], 3);

        printf("initing bars...\n");
        fflush(stdout);
        /* init bars */
        updatebars();
        updatestatus();

        printf("initing netwmcheck...\n");
        fflush(stdout);
        /* supporting window for NetWMCheck */
        wmcheckwin = XCreateSimpleWindow(dpy, root, 0, 0, 1, 1, 0, 0, 0);
        XChangeProperty(dpy, wmcheckwin, netatom[NetWMCheck], XA_WINDOW, 32,
                        PropModeReplace, (unsigned char *)&wmcheckwin, 1);
        XChangeProperty(dpy, wmcheckwin, netatom[NetWMName], utf8string, 8,
                        PropModeReplace, (unsigned char *)"dwm", 3);
        XChangeProperty(dpy, root, netatom[NetWMCheck], XA_WINDOW, 32,
                        PropModeReplace, (unsigned char *)&wmcheckwin, 1);

        printf("initing ewmh support...\n");
        fflush(stdout);
        /* EWMH support per view */
        XChangeProperty(dpy, root, netatom[NetSupported], XA_ATOM, 32,
                        PropModeReplace, (unsigned char *)netatom, NetLast);
        XDeleteProperty(dpy, root, netatom[NetClientList]);

        printf("initing events...\n");
        fflush(stdout);
	/* select events */
	wa.cursor = cursor[CurNormal]->cursor;
	wa.event_mask = SubstructureRedirectMask|SubstructureNotifyMask
		|ButtonPressMask|PointerMotionMask|EnterWindowMask
		|LeaveWindowMask|StructureNotifyMask|PropertyChangeMask;
	XChangeWindowAttributes(dpy, root, CWEventMask|CWCursor, &wa);
	XSelectInput(dpy, root, wa.event_mask);
	grabkeys();
	focus(NULL);
}

void
seturgent(Client *c, int urg)
{
	XWMHints *wmh;

	c->isurgent = urg;
	if (!(wmh = XGetWMHints(dpy, c->win)))
		return;
	wmh->flags = urg ? (wmh->flags | XUrgencyHint) : (wmh->flags & ~XUrgencyHint);
	XSetWMHints(dpy, c->win, wmh);
	XFree(wmh);
}

void
showhide(Client *c)
{
	if (!c)
		return;
	if (ISVISIBLE(c)) {
		/* show clients top down */
		XMoveWindow(dpy, c->win, c->x, c->y);
		if ((!c->mon->lt[c->mon->sellt]->arrange || c->isfloating) && !c->isfullscreen)
			resize(c, c->x, c->y, c->w, c->h, 0);
		showhide(c->snext);
	} else {
		/* hide clients bottom up */
		showhide(c->snext);
		XMoveWindow(dpy, c->win, WIDTH(c) * -2, c->y);
	}
}

void exec_cmd(char** argv) {
  struct sigaction sa;
  if (fork() == 0) {
    if (dpy)
      close(ConnectionNumber(dpy));
    setsid();

    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sa.sa_handler = SIG_DFL;
    sigaction(SIGCHLD, &sa, NULL);

    execvp(argv[0], argv);
    die("dwm: execvp '%s' failed:", argv[0]);
  }
}

void spawn(const Arg *arg) {
  if (arg->v == dmenucmd)
    dmenumon[0] = '0' + selmon->num;
  exec_cmd((char **) arg->v);
}

void
tag(const Arg *arg)
{
	if (selmon->sel && arg->ui & TAGMASK) {
		selmon->sel->tags = arg->ui & TAGMASK;
		focus(NULL);
		arrange(selmon);
	}
}

SCM
scm_tag(SCM num) {
  const Arg arg = {.ui = scm_to_uint(num)};
  tag(&arg);
  return SCM_BOOL_T;
}

void
tagmon(const Arg *arg)
{
	if (!selmon->sel || !mons->next)
		return;
	sendmon(selmon->sel, dirtomon(arg->i));
}

SCM
scm_tagmon(SCM num) {
  const Arg arg = {.i = scm_to_int(num)};
  tagmon(&arg);
  return SCM_BOOL_T;
}

void
tile(Monitor *m)
{
	unsigned int i, n, h, mw, my, ty;
	Client *c;

	for (n = 0, c = nexttiled(m->clients); c; c = nexttiled(c->next), n++);
	if (n == 0)
		return;

	if (n > m->nmaster)
		mw = m->nmaster ? m->ww * m->mfact : 0;
	else
		mw = m->ww;
	for (i = my = ty = 0, c = nexttiled(m->clients); c; c = nexttiled(c->next), i++)
		if (i < m->nmaster) {
			h = (m->wh - my) / (MIN(n, m->nmaster) - i);
			resize(c, m->wx, m->wy + my, mw - (2*c->bw), h - (2*c->bw), 0);
			if (my + HEIGHT(c) < m->wh)
				my += HEIGHT(c);
		} else {
			h = (m->wh - ty) / (n - i);
			resize(c, m->wx + mw, m->wy + ty, m->ww - mw - (2*c->bw), h - (2*c->bw), 0);
			if (ty + HEIGHT(c) < m->wh)
				ty += HEIGHT(c);
		}
}

SCM
scm_tile(SCM monitor) {
  printf("Unwraping monitor\n");
  fflush(stdout);
  Monitor *m = scm_to_monitor(monitor);

  printf("Calling tile\n");
  fflush(stdout);
  tile(m);

  printf("Returning successfully\n");
  fflush(stdout);
  return SCM_UNSPECIFIED;
}

void
togglebar(const Arg *arg)
{
	selmon->showbar = !selmon->showbar;
	updatebarpos(selmon);
	XMoveResizeWindow(dpy, selmon->barwin, selmon->wx, selmon->by, selmon->ww, bh);
	arrange(selmon);
}

SCM
scm_togglebar() {
  togglebar(NULL);
  return SCM_BOOL_T;
}

void
togglefloating(const Arg *arg)
{
	if (!selmon->sel)
		return;
	if (selmon->sel->isfullscreen) /* no support for fullscreen windows */
		return;
	selmon->sel->isfloating = !selmon->sel->isfloating || selmon->sel->isfixed;
	if (selmon->sel->isfloating)
		resize(selmon->sel, selmon->sel->x, selmon->sel->y,
			selmon->sel->w, selmon->sel->h, 0);
	arrange(selmon);
}

SCM
scm_togglefloating() {
  togglefloating(NULL);
  return SCM_BOOL_T;
}

void
toggletag(const Arg *arg)
{
	unsigned int newtags;

	if (!selmon->sel)
		return;
	newtags = selmon->sel->tags ^ (arg->ui & TAGMASK);
	if (newtags) {
		selmon->sel->tags = newtags;
		focus(NULL);
		arrange(selmon);
	}
}

SCM
scm_toggletag(SCM num) {
  const Arg arg = {.ui = scm_to_uint(num)};
  toggletag(&arg);
  return SCM_BOOL_T;
}

void
toggleview(const Arg *arg)
{
	unsigned int newtagset = selmon->tagset[selmon->seltags] ^ (arg->ui & TAGMASK);

	if (newtagset) {
		selmon->tagset[selmon->seltags] = newtagset;
		focus(NULL);
		arrange(selmon);
	}
}

SCM
scm_toggleview(SCM num) {
  const Arg arg = {.ui = scm_to_uint(num)};
  toggleview(&arg);
  return SCM_BOOL_T;
}

void
unfocus(Client *c, int setfocus)
{
	if (!c)
		return;
	grabbuttons(c, 0);
	XSetWindowBorder(dpy, c->win, scheme[SchemeNorm][ColBorder].pixel);
	if (setfocus) {
		XSetInputFocus(dpy, root, RevertToPointerRoot, CurrentTime);
		XDeleteProperty(dpy, root, netatom[NetActiveWindow]);
	}
}

void
unmanage(Client *c, int destroyed)
{
	Monitor *m = c->mon;
	XWindowChanges wc;

	detach(c);
	detachstack(c);
	if (!destroyed) {
		wc.border_width = c->oldbw;
		XGrabServer(dpy); /* avoid race conditions */
		XSetErrorHandler(xerrordummy);
		XSelectInput(dpy, c->win, NoEventMask);
		XConfigureWindow(dpy, c->win, CWBorderWidth, &wc); /* restore border */
		XUngrabButton(dpy, AnyButton, AnyModifier, c->win);
		setclientstate(c, WithdrawnState);
		XSync(dpy, False);
		XSetErrorHandler(xerror);
		XUngrabServer(dpy);
	}
	free(c);
	focus(NULL);
	updateclientlist();
	arrange(m);
}

void
unmapnotify(XEvent *e)
{
	Client *c;
	XUnmapEvent *ev = &e->xunmap;

	if ((c = wintoclient(ev->window))) {
		if (ev->send_event)
			setclientstate(c, WithdrawnState);
		else
			unmanage(c, 0);
	}
}

void
updatebars(void)
{
	Monitor *m;
	XSetWindowAttributes wa = {
		.override_redirect = True,
		.background_pixmap = ParentRelative,
		.event_mask = ButtonPressMask|ExposureMask
	};
	XClassHint ch = {"dwm", "dwm"};
	for (m = mons; m; m = m->next) {
		if (m->barwin)
			continue;
		m->barwin = XCreateWindow(dpy, root, m->wx, m->by, m->ww, bh, 0, DefaultDepth(dpy, screen),
				CopyFromParent, DefaultVisual(dpy, screen),
				CWOverrideRedirect|CWBackPixmap|CWEventMask, &wa);
		XDefineCursor(dpy, m->barwin, cursor[CurNormal]->cursor);
		XMapRaised(dpy, m->barwin);
		XSetClassHint(dpy, m->barwin, &ch);
	}
}

void
updatebarpos(Monitor *m)
{
	m->wy = m->my;
	m->wh = m->mh;
	if (m->showbar) {
		m->wh -= bh;
		m->by = m->topbar ? m->wy : m->wy + m->wh;
		m->wy = m->topbar ? m->wy + bh : m->wy;
	} else
		m->by = -bh;
}

void
updateclientlist(void)
{
	Client *c;
	Monitor *m;

	XDeleteProperty(dpy, root, netatom[NetClientList]);
	for (m = mons; m; m = m->next)
		for (c = m->clients; c; c = c->next)
			XChangeProperty(dpy, root, netatom[NetClientList],
				XA_WINDOW, 32, PropModeAppend,
				(unsigned char *) &(c->win), 1);
}

int
updategeom(void)
{
	int dirty = 0;

#ifdef XINERAMA
  printf("Running XINERAMA def\n");
  fflush(stdout);
  if (XineramaIsActive(dpy)) {
    printf("Running XINERAMA code\n");
    fflush(stdout);
    int i, j, n, nn;
    Client *c;
    Monitor *m;
    XineramaScreenInfo *info = XineramaQueryScreens(dpy, &nn);
    XineramaScreenInfo *unique = NULL;

    for (n = 0, m = mons; m; m = m->next, n++)
      ;
    printf("Unique\n");
    fflush(stdout);
    /* only consider unique geometries as separate screens */
    unique = ecalloc(nn, sizeof(XineramaScreenInfo));
    for (i = 0, j = 0; i < nn; i++)
      if (isuniquegeom(unique, j, &info[i]))
        memcpy(&unique[j++], &info[i], sizeof(XineramaScreenInfo));
    XFree(info);
    nn = j;

    printf("New monitors\n");
    fflush(stdout);
    /* new monitors if nn > n */
    for (i = n; i < nn; i++) {
      for (m = mons; m && m->next; m = m->next)
        ;
      if (m)
        m->next = createmon();
      else
        mons = createmon();
    }
    printf("Check barpos\n");
    fflush(stdout);
    for (i = 0, m = mons; i < nn && m; m = m->next, i++)
      if (i >= n || unique[i].x_org != m->mx || unique[i].y_org != m->my ||
          unique[i].width != m->mw || unique[i].height != m->mh) {
        dirty = 1;
        m->num = i;
        m->mx = m->wx = unique[i].x_org;
        m->my = m->wy = unique[i].y_org;
        m->mw = m->ww = unique[i].width;
        m->mh = m->wh = unique[i].height;
        updatebarpos(m);
      }
    printf("Removed monitors\n");
    fflush(stdout);
    /* removed monitors if n > nn */
    for (i = nn; i < n; i++) {
      for (m = mons; m && m->next; m = m->next)
        ;
      while ((c = m->clients)) {
        dirty = 1;
        m->clients = c->next;
        detachstack(c);
        c->mon = mons;
        attach(c);
        attachstack(c);
      }
      if (m == selmon)
        selmon = mons;
      cleanupmon(m);
    }
    printf("Free unique\n");
    fflush(stdout);
    free(unique);
  } else
#endif /* XINERAMA */
  {    /* default monitor setup */
		if (!mons) {
      printf("Creating mon...\n");
      fflush(stdout);
      mons = createmon();
                }
                if (mons->mw != sw || mons->mh != sh) {
                  dirty = 1;
                  mons->mw = mons->ww = sw;
                  mons->mh = mons->wh = sh;
                  printf("Updating bar position...\n");
                  fflush(stdout);
                  updatebarpos(mons);
                }
  }
        if (dirty) {
          printf("Calling wintomon...\n");
          fflush(stdout);
          selmon = mons;
          selmon = wintomon(root);
        }
        return dirty;
}

void
updatenumlockmask(void)
{
	unsigned int i, j;
	XModifierKeymap *modmap;

	numlockmask = 0;
	modmap = XGetModifierMapping(dpy);
	for (i = 0; i < 8; i++)
		for (j = 0; j < modmap->max_keypermod; j++)
			if (modmap->modifiermap[i * modmap->max_keypermod + j]
				== XKeysymToKeycode(dpy, XK_Num_Lock))
				numlockmask = (1 << i);
	XFreeModifiermap(modmap);
}

void
updatesizehints(Client *c)
{
	long msize;
	XSizeHints size;

	if (!XGetWMNormalHints(dpy, c->win, &size, &msize))
		/* size is uninitialized, ensure that size.flags aren't used */
		size.flags = PSize;
	if (size.flags & PBaseSize) {
		c->basew = size.base_width;
		c->baseh = size.base_height;
	} else if (size.flags & PMinSize) {
		c->basew = size.min_width;
		c->baseh = size.min_height;
	} else
		c->basew = c->baseh = 0;
	if (size.flags & PResizeInc) {
		c->incw = size.width_inc;
		c->inch = size.height_inc;
	} else
		c->incw = c->inch = 0;
	if (size.flags & PMaxSize) {
		c->maxw = size.max_width;
		c->maxh = size.max_height;
	} else
		c->maxw = c->maxh = 0;
	if (size.flags & PMinSize) {
		c->minw = size.min_width;
		c->minh = size.min_height;
	} else if (size.flags & PBaseSize) {
		c->minw = size.base_width;
		c->minh = size.base_height;
	} else
		c->minw = c->minh = 0;
	if (size.flags & PAspect) {
		c->mina = (float)size.min_aspect.y / size.min_aspect.x;
		c->maxa = (float)size.max_aspect.x / size.max_aspect.y;
	} else
		c->maxa = c->mina = 0.0;
	c->isfixed = (c->maxw && c->maxh && c->maxw == c->minw && c->maxh == c->minh);
	c->hintsvalid = 1;
}

void
updatestatus(void)
{
	if (!gettextprop(root, XA_WM_NAME, stext, sizeof(stext)))
		strcpy(stext, "dwm-"VERSION);
	drawbar(selmon);
}

void
updatetitle(Client *c)
{
	if (!gettextprop(c->win, netatom[NetWMName], c->name, sizeof c->name))
		gettextprop(c->win, XA_WM_NAME, c->name, sizeof c->name);
	if (c->name[0] == '\0') /* hack to mark broken clients */
		strcpy(c->name, broken);
}

void
updatewindowtype(Client *c)
{
	Atom state = getatomprop(c, netatom[NetWMState]);
	Atom wtype = getatomprop(c, netatom[NetWMWindowType]);

	if (state == netatom[NetWMFullscreen])
		setfullscreen(c, 1);
	if (wtype == netatom[NetWMWindowTypeDialog])
		c->isfloating = 1;
}

void
updatewmhints(Client *c)
{
	XWMHints *wmh;

	if ((wmh = XGetWMHints(dpy, c->win))) {
		if (c == selmon->sel && wmh->flags & XUrgencyHint) {
			wmh->flags &= ~XUrgencyHint;
			XSetWMHints(dpy, c->win, wmh);
		} else
			c->isurgent = (wmh->flags & XUrgencyHint) ? 1 : 0;
		if (wmh->flags & InputHint)
			c->neverfocus = !wmh->input;
		else
			c->neverfocus = 0;
		XFree(wmh);
	}
}

void
view(const Arg *arg)
{
	if ((arg->ui & TAGMASK) == selmon->tagset[selmon->seltags])
		return;
	selmon->seltags ^= 1; /* toggle sel tagset */
	if (arg->ui & TAGMASK)
		selmon->tagset[selmon->seltags] = arg->ui & TAGMASK;
	focus(NULL);
	arrange(selmon);
}

SCM
scm_view(SCM num) {
  const Arg arg = {.ui = scm_to_uint(num)};
  view(&arg);
  return SCM_BOOL_T;
}

Client *
wintoclient(Window w)
{
	Client *c;
	Monitor *m;

	for (m = mons; m; m = m->next)
		for (c = m->clients; c; c = c->next)
			if (c->win == w)
				return c;
	return NULL;
}

Monitor *
wintomon(Window w)
{
	int x, y;
	Client *c;
	Monitor *m;

	if (w == root && getrootptr(&x, &y))
		return recttomon(x, y, 1, 1);
	for (m = mons; m; m = m->next)
		if (w == m->barwin)
			return m;
	if ((c = wintoclient(w)))
		return c->mon;
	return selmon;
}

/* There's no way to check accesses to destroyed windows, thus those cases are
 * ignored (especially on UnmapNotify's). Other types of errors call Xlibs
 * default error handler, which may call exit. */
int
xerror(Display *dpy, XErrorEvent *ee)
{
	if (ee->error_code == BadWindow
	|| (ee->request_code == X_SetInputFocus && ee->error_code == BadMatch)
	|| (ee->request_code == X_PolyText8 && ee->error_code == BadDrawable)
	|| (ee->request_code == X_PolyFillRectangle && ee->error_code == BadDrawable)
	|| (ee->request_code == X_PolySegment && ee->error_code == BadDrawable)
	|| (ee->request_code == X_ConfigureWindow && ee->error_code == BadMatch)
	|| (ee->request_code == X_GrabButton && ee->error_code == BadAccess)
	|| (ee->request_code == X_GrabKey && ee->error_code == BadAccess)
	|| (ee->request_code == X_CopyArea && ee->error_code == BadDrawable))
		return 0;
	fprintf(stderr, "dwm: fatal error: request code=%d, error code=%d\n",
		ee->request_code, ee->error_code);
	return xerrorxlib(dpy, ee); /* may call exit */
}

int
xerrordummy(Display *dpy, XErrorEvent *ee)
{
	return 0;
}

/* Startup Error handler to check if another window manager
 * is already running. */
int
xerrorstart(Display *dpy, XErrorEvent *ee)
{
	die("dwm: another window manager is already running");
	return -1;
}

void
zoom(const Arg *arg)
{
	Client *c = selmon->sel;

	if (!selmon->lt[selmon->sellt]->arrange || !c || c->isfloating)
		return;
	if (c == nexttiled(selmon->clients) && !(c = nexttiled(c->next)))
		return;
	pop(c);
}

SCM
scm_zoom() {
  zoom(NULL);
  return SCM_BOOL_T;
}

int
main(int argc, char *argv[])
{
  printf("Booting dwm...\n");
  fflush(stdout);
  if (argc == 2 && !strcmp("-v", argv[1]))
    die("dwm-" VERSION);
  else if (argc != 1)
    die("usage: dwm [-v]");
  if (!setlocale(LC_CTYPE, "") || !XSupportsLocale())
    fputs("warning: no locale support\n", stderr);
  if (!(dpy = XOpenDisplay(NULL)))
    die("dwm: cannot open display");
  checkotherwm();
  printf("Setting up dwm...\n");
  fflush(stdout);
  setup();
#ifdef __OpenBSD__
  if (pledge("stdio rpath proc exec", NULL) == -1)
    die("pledge");
#endif /* __OpenBSD__ */
  printf("Scanning...\n");
  fflush(stdout);
  scan();
  printf("Entering main loop...\n");
  fflush(stdout);
  run();
  printf("stoping DWM...\n");
  fflush(stdout);
  cleanup();
  XCloseDisplay(dpy);
  printf("exiting DWM...\n");
  fflush(stdout);
  return EXIT_SUCCESS;
}
