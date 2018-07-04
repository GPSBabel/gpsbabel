/****************************************************************************
** Meta object code from reading C++ file 'processwait.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.9.5)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../gui/processwait.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'processwait.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.9.5. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_ProcessWaitDialog_t {
    QByteArrayData data[11];
    char stringdata0[160];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_ProcessWaitDialog_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_ProcessWaitDialog_t qt_meta_stringdata_ProcessWaitDialog = {
    {
QT_MOC_LITERAL(0, 0, 17), // "ProcessWaitDialog"
QT_MOC_LITERAL(1, 18, 6), // "errorX"
QT_MOC_LITERAL(2, 25, 0), // ""
QT_MOC_LITERAL(3, 26, 22), // "QProcess::ProcessError"
QT_MOC_LITERAL(4, 49, 9), // "finishedX"
QT_MOC_LITERAL(5, 59, 8), // "exitCode"
QT_MOC_LITERAL(6, 68, 20), // "QProcess::ExitStatus"
QT_MOC_LITERAL(7, 89, 23), // "readyReadStandardErrorX"
QT_MOC_LITERAL(8, 113, 24), // "readyReadStandardOutputX"
QT_MOC_LITERAL(9, 138, 8), // "timeoutX"
QT_MOC_LITERAL(10, 147, 12) // "stopClickedX"

    },
    "ProcessWaitDialog\0errorX\0\0"
    "QProcess::ProcessError\0finishedX\0"
    "exitCode\0QProcess::ExitStatus\0"
    "readyReadStandardErrorX\0"
    "readyReadStandardOutputX\0timeoutX\0"
    "stopClickedX"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_ProcessWaitDialog[] = {

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
       1,    1,   44,    2, 0x08 /* Private */,
       4,    2,   47,    2, 0x08 /* Private */,
       7,    0,   52,    2, 0x08 /* Private */,
       8,    0,   53,    2, 0x08 /* Private */,
       9,    0,   54,    2, 0x08 /* Private */,
      10,    0,   55,    2, 0x08 /* Private */,

 // slots: parameters
    QMetaType::Void, 0x80000000 | 3,    2,
    QMetaType::Void, QMetaType::Int, 0x80000000 | 6,    5,    2,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void ProcessWaitDialog::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        ProcessWaitDialog *_t = static_cast<ProcessWaitDialog *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->errorX((*reinterpret_cast< QProcess::ProcessError(*)>(_a[1]))); break;
        case 1: _t->finishedX((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< QProcess::ExitStatus(*)>(_a[2]))); break;
        case 2: _t->readyReadStandardErrorX(); break;
        case 3: _t->readyReadStandardOutputX(); break;
        case 4: _t->timeoutX(); break;
        case 5: _t->stopClickedX(); break;
        default: ;
        }
    }
}

const QMetaObject ProcessWaitDialog::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_ProcessWaitDialog.data,
      qt_meta_data_ProcessWaitDialog,  qt_static_metacall, nullptr, nullptr}
};


const QMetaObject *ProcessWaitDialog::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *ProcessWaitDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_ProcessWaitDialog.stringdata0))
        return static_cast<void*>(this);
    return QDialog::qt_metacast(_clname);
}

int ProcessWaitDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
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
QT_WARNING_POP
QT_END_MOC_NAMESPACE
