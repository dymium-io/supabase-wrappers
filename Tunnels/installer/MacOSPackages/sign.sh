#!/bin/bash

productsign --sign "Developer ID Installer: Dymium Inc (RC7F4R4R28)" ./build/DymiumInstaller.pkg ./DymiumInstaller.pkg
retval=$?
[ $retval -ne 0 ] && {
    echo "signing failed with error code $retval"
    exit $retval
}

xcrun notarytool submit ./DymiumInstaller.pkg --keychain-profile "AC_PASSWORD" --wait
retval=$?
[ $retval -ne 0 ] && {
    echo "notary submission failed with error code $retval"
    exit $retval
}

xcrun stapler staple DymiumInstaller.pkg
retval=$?
[ $retval -ne 0 ] && {
    echo "stapling failed with error code $retval"
    exit $retval
}

spctl --assess -vv --type install ./DymiumInstaller.pkg

