// lifted from  joshtronic
import React, { Component } from "react";
import PropTypes from "prop-types";
import Form from 'react-bootstrap/Form'

class Autocomplete extends Component {
    static propTypes = {
        suggestions: PropTypes.instanceOf(Array)
    };

    static defaultProps = {
        suggestions: []
    };

    constructor(props) {
        super(props);

        this.state = {
            activeSuggestion: 0,
            filteredSuggestions: [],
            showSuggestions: false,

        };
    }

    onChange = e => {
        const { suggestions } = this.props;

        const filteredSuggestions = suggestions.filter(
            suggestion =>
                suggestion.toLowerCase().indexOf(e.target.value.toLowerCase()) > -1
        );

        this.setState({
            activeSuggestion: 0,
            filteredSuggestions,
            showSuggestions: true,
        });

        this.props.onChange(e)
    };

    onClick = e => {
       
        this.setState({
            activeSuggestion: 0,
            filteredSuggestions: [],
            showSuggestions: false,
        });

        this.props.onChange( {target: {value: e.currentTarget.innerText}} )
    };

    onBlur = e => {
        return
        /*
        this.setState({
            showSuggestions: false,
        })
        */
    }
    onFocus = e => {
        this.setState({
            showSuggestions: true,
        })

    }
    onKeyDown = e => {
        const { activeSuggestion, filteredSuggestions } = this.state;

        if (e.keyCode === 13) {
         
            this.setState({
                activeSuggestion: 0,
                showSuggestions: false,
            });
            this.props.onChange( {target: {value: filteredSuggestions[activeSuggestion]}} )
            e.preventDefault();
            e.stopPropagation()
            return false
        }
        else if (e.keyCode === 38) {
            if (activeSuggestion === 0) {
                return;
            }
            this.setState({ activeSuggestion: activeSuggestion - 1 });
        }
        else if (e.keyCode === 40) {
            if (activeSuggestion - 1 === filteredSuggestions.length) {
                return;
            }

            this.setState({ activeSuggestion: activeSuggestion + 1 });
        }
    };

    render() {
        const {
            onChange,
            onKeyDown,
            state: {
                activeSuggestion,
                filteredSuggestions,
                showSuggestions,

            }
        } = this;

        let suggestionsListComponent;

        if (showSuggestions && this.props.value) {
            if (filteredSuggestions.length) {
                suggestionsListComponent = (
                    <ul className="suggestions" >
                        {filteredSuggestions.map((suggestion, index) => {
                            let className;

                            // Flag the active suggestion with a class
                            if (index === activeSuggestion) {
                                className = "suggestion-active";
                            }

                            return (
                                <li
                                    className={className}
                                    key={suggestion}
                                    onClick={this.onClick}
                                    onSelect={this.onClick}
                                >
                                    {suggestion}
                                </li>
                            );
                        })}
                    </ul>
                );
            } else {
                suggestionsListComponent = (
                    <div className="no-suggestions">
                    </div>
                );
            }
        }

        return (
            <Form.Group controlId={this.props.name + "_control"} className="text-left" >
                <label forhtml={this.props.name} >{this.props.label} </label>
                <input
                    name={this.props.name}
                    id={this.props.id}
                    type="text"
                    onChange={onChange}
                    onKeyDown={onKeyDown}
                    autoComplete="off"
                    placeholder={this.props.placeholder}
                    value={this.props.value}
                    className="form-control form-control-sm"
                    pattern={this.props.pattern}
                    required={this.props.required}
                    onBlur={this.onBlur}
                    onFocus={this.onFocus}
                    ref={this.props.reference}
                />
                {suggestionsListComponent}
                <Form.Control.Feedback >{this.props.valid}</Form.Control.Feedback>
                <Form.Control.Feedback type="invalid" >
                    {this.props.invalid}
                </Form.Control.Feedback>
            </Form.Group>
        );
    }
}

export default Autocomplete;