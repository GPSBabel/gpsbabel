/****************************************************************************
** Meta object code from reading C++ file 'map.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.9.5)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../gui/map.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#include <QtCore/QList>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'map.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.9.5. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_MarkerClicker_t {
    QByteArrayData data[9];
    char stringdata0[61];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_MarkerClicker_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_MarkerClicker_t qt_meta_stringdata_MarkerClicker = {
    {
QT_MOC_LITERAL(0, 0, 13), // "MarkerClicker"
QT_MOC_LITERAL(1, 14, 13), // "markerClicked"
QT_MOC_LITERAL(2, 28, 0), // ""
QT_MOC_LITERAL(3, 29, 1), // "t"
QT_MOC_LITERAL(4, 31, 1), // "i"
QT_MOC_LITERAL(5, 33, 7), // "logTime"
QT_MOC_LITERAL(6, 41, 1), // "s"
QT_MOC_LITERAL(7, 43, 8), // "clickedX"
QT_MOC_LITERAL(8, 52, 8) // "logTimeX"

    },
    "MarkerClicker\0markerClicked\0\0t\0i\0"
    "logTime\0s\0clickedX\0logTimeX"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_MarkerClicker[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: name, argc, parameters, tag, flags
       1,    2,   34,    2, 0x06 /* Public */,
       5,    1,   39,    2, 0x06 /* Public */,

 // slots: name, argc, parameters, tag, flags
       7,    2,   42,    2, 0x0a /* Public */,
       8,    1,   47,    2, 0x0a /* Public */,

 // signals: parameters
    QMetaType::Void, QMetaType::Int, QMetaType::Int,    3,    4,
    QMetaType::Void, QMetaType::QString,    6,

 // slots: parameters
    QMetaType::Void, QMetaType::Int, QMetaType::Int,    3,    4,
    QMetaType::Void, QMetaType::QString,    6,

       0        // eod
};

void MarkerClicker::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        MarkerClicker *_t = static_cast<MarkerClicker *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->markerClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 1: _t->logTime((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: _t->clickedX((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 3: _t->logTimeX((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        default: ;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        {
            typedef void (MarkerClicker::*_t)(int , int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&MarkerClicker::markerClicked)) {
                *result = 0;
                return;
            }
        }
        {
            typedef void (MarkerClicker::*_t)(const QString & );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&MarkerClicker::logTime)) {
                *result = 1;
                return;
            }
        }
    }
}

const QMetaObject MarkerClicker::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MarkerClicker.data,
      qt_meta_data_MarkerClicker,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *MarkerClicker::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *MarkerClicker::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_MarkerClicker.stringdata0))
        return static_cast<void*>(this);
    return QObject::qt_metacast(_clname);
}

int MarkerClicker::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 4)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void MarkerClicker::markerClicked(int _t1, int _t2)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MarkerClicker::logTime(const QString & _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
struct qt_meta_stringdata_Map_t {
    QByteArrayData data[34];
    char stringdata0[400];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_Map_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_Map_t qt_meta_stringdata_Map = {
    {
QT_MOC_LITERAL(0, 0, 3), // "Map"
QT_MOC_LITERAL(1, 4, 15), // "waypointClicked"
QT_MOC_LITERAL(2, 20, 0), // ""
QT_MOC_LITERAL(3, 21, 1), // "i"
QT_MOC_LITERAL(4, 23, 12), // "trackClicked"
QT_MOC_LITERAL(5, 36, 12), // "routeClicked"
QT_MOC_LITERAL(6, 49, 11), // "showGpxData"
QT_MOC_LITERAL(7, 61, 10), // "showTracks"
QT_MOC_LITERAL(8, 72, 15), // "QList<GpxTrack>"
QT_MOC_LITERAL(9, 88, 6), // "tracks"
QT_MOC_LITERAL(10, 95, 13), // "hideAllTracks"
QT_MOC_LITERAL(11, 109, 18), // "setTrackVisibility"
QT_MOC_LITERAL(12, 128, 4), // "show"
QT_MOC_LITERAL(13, 133, 13), // "showWaypoints"
QT_MOC_LITERAL(14, 147, 18), // "QList<GpxWaypoint>"
QT_MOC_LITERAL(15, 166, 9), // "waypoints"
QT_MOC_LITERAL(16, 176, 16), // "hideAllWaypoints"
QT_MOC_LITERAL(17, 193, 21), // "setWaypointVisibility"
QT_MOC_LITERAL(18, 215, 10), // "showRoutes"
QT_MOC_LITERAL(19, 226, 15), // "QList<GpxRoute>"
QT_MOC_LITERAL(20, 242, 6), // "routes"
QT_MOC_LITERAL(21, 249, 13), // "hideAllRoutes"
QT_MOC_LITERAL(22, 263, 18), // "setRouteVisibility"
QT_MOC_LITERAL(23, 282, 13), // "loadFinishedX"
QT_MOC_LITERAL(24, 296, 13), // "markerClicked"
QT_MOC_LITERAL(25, 310, 1), // "t"
QT_MOC_LITERAL(26, 312, 5), // "panTo"
QT_MOC_LITERAL(27, 318, 6), // "LatLng"
QT_MOC_LITERAL(28, 325, 3), // "loc"
QT_MOC_LITERAL(29, 329, 19), // "setWaypointColorRed"
QT_MOC_LITERAL(30, 349, 20), // "setWaypointColorBlue"
QT_MOC_LITERAL(31, 370, 10), // "frameTrack"
QT_MOC_LITERAL(32, 381, 10), // "frameRoute"
QT_MOC_LITERAL(33, 392, 7) // "logTime"

    },
    "Map\0waypointClicked\0\0i\0trackClicked\0"
    "routeClicked\0showGpxData\0showTracks\0"
    "QList<GpxTrack>\0tracks\0hideAllTracks\0"
    "setTrackVisibility\0show\0showWaypoints\0"
    "QList<GpxWaypoint>\0waypoints\0"
    "hideAllWaypoints\0setWaypointVisibility\0"
    "showRoutes\0QList<GpxRoute>\0routes\0"
    "hideAllRoutes\0setRouteVisibility\0"
    "loadFinishedX\0markerClicked\0t\0panTo\0"
    "LatLng\0loc\0setWaypointColorRed\0"
    "setWaypointColorBlue\0frameTrack\0"
    "frameRoute\0logTime"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_Map[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
      21,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       3,       // signalCount

 // signals: name, argc, parameters, tag, flags
       1,    1,  119,    2, 0x06 /* Public */,
       4,    1,  122,    2, 0x06 /* Public */,
       5,    1,  125,    2, 0x06 /* Public */,

 // slots: name, argc, parameters, tag, flags
       6,    0,  128,    2, 0x0a /* Public */,
       7,    1,  129,    2, 0x0a /* Public */,
      10,    0,  132,    2, 0x0a /* Public */,
      11,    2,  133,    2, 0x0a /* Public */,
      13,    1,  138,    2, 0x0a /* Public */,
      16,    0,  141,    2, 0x0a /* Public */,
      17,    2,  142,    2, 0x0a /* Public */,
      18,    1,  147,    2, 0x0a /* Public */,
      21,    0,  150,    2, 0x0a /* Public */,
      22,    2,  151,    2, 0x0a /* Public */,
      23,    1,  156,    2, 0x0a /* Public */,
      24,    2,  159,    2, 0x0a /* Public */,
      26,    1,  164,    2, 0x0a /* Public */,
      29,    1,  167,    2, 0x0a /* Public */,
      30,    1,  170,    2, 0x0a /* Public */,
      31,    1,  173,    2, 0x0a /* Public */,
      32,    1,  176,    2, 0x0a /* Public */,
      33,    1,  179,    2, 0x0a /* Public */,

 // signals: parameters
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,

 // slots: parameters
    QMetaType::Void,
    QMetaType::Void, 0x80000000 | 8,    9,
    QMetaType::Void,
    QMetaType::Void, QMetaType::Int, QMetaType::Bool,    3,   12,
    QMetaType::Void, 0x80000000 | 14,   15,
    QMetaType::Void,
    QMetaType::Void, QMetaType::Int, QMetaType::Bool,    3,   12,
    QMetaType::Void, 0x80000000 | 19,   20,
    QMetaType::Void,
    QMetaType::Void, QMetaType::Int, QMetaType::Bool,    3,   12,
    QMetaType::Void, QMetaType::Bool,    2,
    QMetaType::Void, QMetaType::Int, QMetaType::Int,   25,    3,
    QMetaType::Void, 0x80000000 | 27,   28,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::QString,    2,

       0        // eod
};

void Map::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Map *_t = static_cast<Map *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->waypointClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: _t->trackClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: _t->routeClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: _t->showGpxData(); break;
        case 4: _t->showTracks((*reinterpret_cast< const QList<GpxTrack>(*)>(_a[1]))); break;
        case 5: _t->hideAllTracks(); break;
        case 6: _t->setTrackVisibility((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 7: _t->showWaypoints((*reinterpret_cast< const QList<GpxWaypoint>(*)>(_a[1]))); break;
        case 8: _t->hideAllWaypoints(); break;
        case 9: _t->setWaypointVisibility((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 10: _t->showRoutes((*reinterpret_cast< const QList<GpxRoute>(*)>(_a[1]))); break;
        case 11: _t->hideAllRoutes(); break;
        case 12: _t->setRouteVisibility((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 13: _t->loadFinishedX((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 14: _t->markerClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 15: _t->panTo((*reinterpret_cast< const LatLng(*)>(_a[1]))); break;
        case 16: _t->setWaypointColorRed((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 17: _t->setWaypointColorBlue((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 18: _t->frameTrack((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 19: _t->frameRoute((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 20: _t->logTime((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        default: ;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        {
            typedef void (Map::*_t)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&Map::waypointClicked)) {
                *result = 0;
                return;
            }
        }
        {
            typedef void (Map::*_t)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&Map::trackClicked)) {
                *result = 1;
                return;
            }
        }
        {
            typedef void (Map::*_t)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&Map::routeClicked)) {
                *result = 2;
                return;
            }
        }
    }
}

const QMetaObject Map::staticMetaObject = {
    { &QWebEngineView::staticMetaObject, qt_meta_stringdata_Map.data,
      qt_meta_data_Map,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *Map::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *Map::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_Map.stringdata0))
        return static_cast<void*>(this);
    return QWebEngineView::qt_metacast(_clname);
}

int Map::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWebEngineView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 21)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 21;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 21)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 21;
    }
    return _id;
}

// SIGNAL 0
void Map::waypointClicked(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Map::trackClicked(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Map::routeClicked(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_WARNING_POP
QT_END_MOC_NAMESPACE
