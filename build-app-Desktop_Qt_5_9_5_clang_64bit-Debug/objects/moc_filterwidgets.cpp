/****************************************************************************
** Meta object code from reading C++ file 'filterwidgets.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.9.5)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../gui/filterwidgets.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'filterwidgets.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.9.5. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_CheckEnabler_t {
    QByteArrayData data[3];
    char stringdata0[33];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_CheckEnabler_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_CheckEnabler_t qt_meta_stringdata_CheckEnabler = {
    {
QT_MOC_LITERAL(0, 0, 12), // "CheckEnabler"
QT_MOC_LITERAL(1, 13, 18), // "checkStatusChanged"
QT_MOC_LITERAL(2, 32, 0) // ""

    },
    "CheckEnabler\0checkStatusChanged\0"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_CheckEnabler[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags
       1,    0,   19,    2, 0x0a /* Public */,

 // slots: parameters
    QMetaType::Void,

       0        // eod
};

void CheckEnabler::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        CheckEnabler *_t = static_cast<CheckEnabler *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->checkStatusChanged(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObject CheckEnabler::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_CheckEnabler.data,
      qt_meta_data_CheckEnabler,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *CheckEnabler::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *CheckEnabler::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_CheckEnabler.stringdata0))
        return static_cast<void*>(this);
    return QObject::qt_metacast(_clname);
}

int CheckEnabler::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 1)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 1;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 1)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 1;
    }
    return _id;
}
struct qt_meta_stringdata_TrackWidget_t {
    QByteArrayData data[8];
    char stringdata0[85];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_TrackWidget_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_TrackWidget_t qt_meta_stringdata_TrackWidget = {
    {
QT_MOC_LITERAL(0, 0, 11), // "TrackWidget"
QT_MOC_LITERAL(1, 12, 11), // "mergeCheckX"
QT_MOC_LITERAL(2, 24, 0), // ""
QT_MOC_LITERAL(3, 25, 11), // "otherCheckX"
QT_MOC_LITERAL(4, 37, 10), // "splitDateX"
QT_MOC_LITERAL(5, 48, 10), // "splitTimeX"
QT_MOC_LITERAL(6, 59, 14), // "splitDistanceX"
QT_MOC_LITERAL(7, 74, 10) // "packCheckX"

    },
    "TrackWidget\0mergeCheckX\0\0otherCheckX\0"
    "splitDateX\0splitTimeX\0splitDistanceX\0"
    "packCheckX"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_TrackWidget[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags
       1,    0,   44,    2, 0x08 /* Private */,
       3,    0,   45,    2, 0x08 /* Private */,
       4,    0,   46,    2, 0x08 /* Private */,
       5,    0,   47,    2, 0x08 /* Private */,
       6,    0,   48,    2, 0x08 /* Private */,
       7,    0,   49,    2, 0x08 /* Private */,

 // slots: parameters
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void TrackWidget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        TrackWidget *_t = static_cast<TrackWidget *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->mergeCheckX(); break;
        case 1: _t->otherCheckX(); break;
        case 2: _t->splitDateX(); break;
        case 3: _t->splitTimeX(); break;
        case 4: _t->splitDistanceX(); break;
        case 5: _t->packCheckX(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObject TrackWidget::staticMetaObject = {
    { &FilterWidget::staticMetaObject, qt_meta_stringdata_TrackWidget.data,
      qt_meta_data_TrackWidget,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *TrackWidget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *TrackWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_TrackWidget.stringdata0))
        return static_cast<void*>(this);
    return FilterWidget::qt_metacast(_clname);
}

int TrackWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = FilterWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 6)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 6;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 6)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 6;
    }
    return _id;
}
struct qt_meta_stringdata_WayPtsWidget_t {
    QByteArrayData data[4];
    char stringdata0[41];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_WayPtsWidget_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_WayPtsWidget_t qt_meta_stringdata_WayPtsWidget = {
    {
QT_MOC_LITERAL(0, 0, 12), // "WayPtsWidget"
QT_MOC_LITERAL(1, 13, 12), // "locationsCkX"
QT_MOC_LITERAL(2, 26, 0), // ""
QT_MOC_LITERAL(3, 27, 13) // "shortNamesCkX"

    },
    "WayPtsWidget\0locationsCkX\0\0shortNamesCkX"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_WayPtsWidget[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags
       1,    0,   24,    2, 0x08 /* Private */,
       3,    0,   25,    2, 0x08 /* Private */,

 // slots: parameters
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void WayPtsWidget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        WayPtsWidget *_t = static_cast<WayPtsWidget *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->locationsCkX(); break;
        case 1: _t->shortNamesCkX(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObject WayPtsWidget::staticMetaObject = {
    { &FilterWidget::staticMetaObject, qt_meta_stringdata_WayPtsWidget.data,
      qt_meta_data_WayPtsWidget,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *WayPtsWidget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *WayPtsWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_WayPtsWidget.stringdata0))
        return static_cast<void*>(this);
    return FilterWidget::qt_metacast(_clname);
}

int WayPtsWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = FilterWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 2)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 2;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 2)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 2;
    }
    return _id;
}
struct qt_meta_stringdata_RtTrkWidget_t {
    QByteArrayData data[1];
    char stringdata0[12];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_RtTrkWidget_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_RtTrkWidget_t qt_meta_stringdata_RtTrkWidget = {
    {
QT_MOC_LITERAL(0, 0, 11) // "RtTrkWidget"

    },
    "RtTrkWidget"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_RtTrkWidget[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

void RtTrkWidget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    Q_UNUSED(_o);
    Q_UNUSED(_id);
    Q_UNUSED(_c);
    Q_UNUSED(_a);
}

const QMetaObject RtTrkWidget::staticMetaObject = {
    { &FilterWidget::staticMetaObject, qt_meta_stringdata_RtTrkWidget.data,
      qt_meta_data_RtTrkWidget,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *RtTrkWidget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *RtTrkWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_RtTrkWidget.stringdata0))
        return static_cast<void*>(this);
    return FilterWidget::qt_metacast(_clname);
}

int RtTrkWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = FilterWidget::qt_metacall(_c, _id, _a);
    return _id;
}
struct qt_meta_stringdata_MiscFltWidget_t {
    QByteArrayData data[1];
    char stringdata0[14];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_MiscFltWidget_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_MiscFltWidget_t qt_meta_stringdata_MiscFltWidget = {
    {
QT_MOC_LITERAL(0, 0, 13) // "MiscFltWidget"

    },
    "MiscFltWidget"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_MiscFltWidget[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

void MiscFltWidget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    Q_UNUSED(_o);
    Q_UNUSED(_id);
    Q_UNUSED(_c);
    Q_UNUSED(_a);
}

const QMetaObject MiscFltWidget::staticMetaObject = {
    { &FilterWidget::staticMetaObject, qt_meta_stringdata_MiscFltWidget.data,
      qt_meta_data_MiscFltWidget,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *MiscFltWidget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *MiscFltWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_MiscFltWidget.stringdata0))
        return static_cast<void*>(this);
    return FilterWidget::qt_metacast(_clname);
}

int MiscFltWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = FilterWidget::qt_metacall(_c, _id, _a);
    return _id;
}
QT_WARNING_POP
QT_END_MOC_NAMESPACE
