import React, { useEffect, useState, useRef } from 'react';
import Alert from 'react-bootstrap/Alert'
import Spinner from '@dymium/common/Components/Spinner'
import BootstrapTable from 'react-bootstrap-table-next';
import paginationFactory from 'react-bootstrap-table2-paginator';
import ToolkitProvider, { Search } from 'react-bootstrap-table2-toolkit/dist/react-bootstrap-table2-toolkit';
import 'react-bootstrap-table-next/dist/react-bootstrap-table2.min.css';

const { SearchBar, ClearSearchButton } = Search;

import * as com from '../Common'
import * as types from '@dymium/common/Types/Common'

export default function AssignGroups() {
    const [spinner, setSpinner] = useState(false)
    const [datascopes, setDatascopes] = useState<types.DataScopeInfo[]>([])

    let getDatascopes = () => {
        setSpinner(true)
        com.sendToServer("GET", "/api/getdatascopes",
            null, "",
            resp => {

                resp.json().then(js => {
                    setDatascopes(js)
                 
                })

                setTimeout( () => setSpinner(false), 500)
            },
            resp => {
                console.log("on error")
                setSpinner(false)
            },
            error => {
                console.log("on exception: " + error)
                setSpinner(false)
            })
    }

    useEffect(() => {
        getDatascopes()

    }, [])
    let onEdit = (id) => {
        return e => {

        }
    }

    let columns = [
        {
            dataField: 'id',
            text: 'id',
            hidden: true,
        },
        {
            dataField: 'name',
            text: 'Datascope:',
            headerStyle: { width: '20em' },            
            sort: true,
        },
        {
            dataField: 'groups',
            text: 'Groups:',
            isDummyField: true,
            sort: true
        },
        {
            text: 'Edit',
            dataField: 'edit',
            isDummyField: true,
            formatter: (cell, row, rowIndex, formatExtraData) => {

                return <i className="fas fa-edit ablue" onClick={onEdit(row["id"])} role="button"></i>
            },
            //formatExtraData: { hoverIdx: this.state.hoverIdx },
            headerStyle: { width: '50px' },
            style: { height: '30px' },
            align: 'center'
        },


    ]
    return (
        <div className=" text-left">
            <h5 > Associate groups with Data Scopes <Spinner show={spinner} style={{ width: '28px' }}></Spinner></h5>
            <BootstrapTable id="scaledtable"
                                            condensed
                                            striped bootstrap4 bordered={false}
                                            pagination={paginationFactory()}
                                            keyField='id'
                                            data={datascopes}
                                            columns={columns}
                                        />
        </div>
    )
}