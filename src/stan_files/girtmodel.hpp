/*
    irtstan is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    irtstan is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with irtstan.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.18.0

#include <stan/model/model_header.hpp>

namespace model_girtmodel_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_girtmodel");
    reader.add_event(61, 59, "end", "model_girtmodel");
    return reader;
}

#include <meta_header.hpp>
 class model_girtmodel : public prob_grad {
private:
    int N;
    int M;
    vector<vector<int> > y;
    double D;
public:
    model_girtmodel(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_girtmodel(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;

        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "model_girtmodel_namespace::model_girtmodel";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        // initialize member variables
        try {
            current_statement_begin__ = 4;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "M", "int", context__.to_vec());
            M = int(0);
            vals_i__ = context__.vals_i("M");
            pos__ = 0;
            M = vals_i__[pos__++];
            current_statement_begin__ = 6;
            validate_non_negative_index("y", "N", N);
            validate_non_negative_index("y", "M", M);
            context__.validate_dims("data initialization", "y", "int", context__.to_vec(N,M));
            validate_non_negative_index("y", "N", N);
            validate_non_negative_index("y", "M", M);
            y = std::vector<std::vector<int> >(N,std::vector<int>(M,int(0)));
            vals_i__ = context__.vals_i("y");
            pos__ = 0;
            size_t y_limit_1__ = M;
            for (size_t i_1__ = 0; i_1__ < y_limit_1__; ++i_1__) {
                size_t y_limit_0__ = N;
                for (size_t i_0__ = 0; i_0__ < y_limit_0__; ++i_0__) {
                    y[i_0__][i_1__] = vals_i__[pos__++];
                }
            }
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "D", "double", context__.to_vec());
            D = double(0);
            vals_r__ = context__.vals_r("D");
            pos__ = 0;
            D = vals_r__[pos__++];

            // validate, data variables
            current_statement_begin__ = 4;
            check_greater_or_equal(function__,"N",N,1);
            current_statement_begin__ = 5;
            check_greater_or_equal(function__,"M",M,1);
            current_statement_begin__ = 6;
            current_statement_begin__ = 7;
            // initialize data variables


            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 12;
            validate_non_negative_index("theta", "N", N);
            num_params_r__ += N;
            current_statement_begin__ = 13;
            validate_non_negative_index("a", "M", M);
            num_params_r__ += M;
            current_statement_begin__ = 14;
            validate_non_negative_index("b", "M", M);
            num_params_r__ += M;
            current_statement_begin__ = 15;
            validate_non_negative_index("phi", "N", N);
            num_params_r__ += N;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_girtmodel() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("theta")))
            throw std::runtime_error("variable theta missing");
        vals_r__ = context__.vals_r("theta");
        pos__ = 0U;
        validate_non_negative_index("theta", "N", N);
        context__.validate_dims("initialization", "theta", "double", context__.to_vec(N));
        std::vector<double> theta(N,double(0));
        for (int i0__ = 0U; i0__ < N; ++i0__)
            theta[i0__] = vals_r__[pos__++];
        for (int i0__ = 0U; i0__ < N; ++i0__)
            try {
            writer__.scalar_unconstrain(theta[i0__]);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable theta: ") + e.what());
        }

        if (!(context__.contains_r("a")))
            throw std::runtime_error("variable a missing");
        vals_r__ = context__.vals_r("a");
        pos__ = 0U;
        validate_non_negative_index("a", "M", M);
        context__.validate_dims("initialization", "a", "vector_d", context__.to_vec(M));
        vector_d a(static_cast<Eigen::VectorXd::Index>(M));
        for (int j1__ = 0U; j1__ < M; ++j1__)
            a(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_lb_unconstrain(0,a);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable a: ") + e.what());
        }

        if (!(context__.contains_r("b")))
            throw std::runtime_error("variable b missing");
        vals_r__ = context__.vals_r("b");
        pos__ = 0U;
        validate_non_negative_index("b", "M", M);
        context__.validate_dims("initialization", "b", "vector_d", context__.to_vec(M));
        vector_d b(static_cast<Eigen::VectorXd::Index>(M));
        for (int j1__ = 0U; j1__ < M; ++j1__)
            b(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_unconstrain(b);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable b: ") + e.what());
        }

        if (!(context__.contains_r("phi")))
            throw std::runtime_error("variable phi missing");
        vals_r__ = context__.vals_r("phi");
        pos__ = 0U;
        validate_non_negative_index("phi", "N", N);
        context__.validate_dims("initialization", "phi", "vector_d", context__.to_vec(N));
        vector_d phi(static_cast<Eigen::VectorXd::Index>(N));
        for (int j1__ = 0U; j1__ < N; ++j1__)
            phi(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_lb_unconstrain(0,phi);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable phi: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        typedef T__ local_scalar_t__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        try {
            // model parameters
            stan::io::reader<local_scalar_t__> in__(params_r__,params_i__);

            vector<local_scalar_t__> theta;
            size_t dim_theta_0__ = N;
            theta.reserve(dim_theta_0__);
            for (size_t k_0__ = 0; k_0__ < dim_theta_0__; ++k_0__) {
                if (jacobian__)
                    theta.push_back(in__.scalar_constrain(lp__));
                else
                    theta.push_back(in__.scalar_constrain());
            }

            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  a;
            (void) a;  // dummy to suppress unused var warning
            if (jacobian__)
                a = in__.vector_lb_constrain(0,M,lp__);
            else
                a = in__.vector_lb_constrain(0,M);

            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  b;
            (void) b;  // dummy to suppress unused var warning
            if (jacobian__)
                b = in__.vector_constrain(M,lp__);
            else
                b = in__.vector_constrain(M);

            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  phi;
            (void) phi;  // dummy to suppress unused var warning
            if (jacobian__)
                phi = in__.vector_lb_constrain(0,N,lp__);
            else
                phi = in__.vector_lb_constrain(0,N);


            // transformed parameters



            // validate transformed parameters

            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning

            // model body

            current_statement_begin__ = 20;
            lp_accum__.add(cauchy_log<propto__>(a, 0, 1));
            current_statement_begin__ = 21;
            lp_accum__.add(cauchy_log<propto__>(phi, 0, 1));
            current_statement_begin__ = 22;
            lp_accum__.add(normal_log<propto__>(theta, 0, 1));
            current_statement_begin__ = 23;
            lp_accum__.add(normal_log<propto__>(b, 0, 3));
            current_statement_begin__ = 26;
            for (int k = 1; k <= M; ++k) {

                current_statement_begin__ = 27;
                for (int i = 1; i <= N; ++i) {

                    current_statement_begin__ = 28;
                    lp_accum__.add(bernoulli_logit_log<propto__>(get_base1(get_base1(y,i,"y",1),k,"y",2), (((D * get_base1(a,k,"a",1)) / stan::math::sqrt((1 + (pow(get_base1(phi,i,"phi",1),2) * pow(get_base1(a,k,"a",1),2))))) * (get_base1(theta,i,"theta",1) - get_base1(b,k,"b",1)))));
                }
            }

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("theta");
        names__.push_back("a");
        names__.push_back("b");
        names__.push_back("phi");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(M);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(M);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;

        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__,params_i__);
        static const char* function__ = "model_girtmodel_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        vector<double> theta;
        size_t dim_theta_0__ = N;
        for (size_t k_0__ = 0; k_0__ < dim_theta_0__; ++k_0__) {
            theta.push_back(in__.scalar_constrain());
        }
        vector_d a = in__.vector_lb_constrain(0,M);
        vector_d b = in__.vector_constrain(M);
        vector_d phi = in__.vector_lb_constrain(0,N);
            for (int k_0__ = 0; k_0__ < N; ++k_0__) {
            vars__.push_back(theta[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < M; ++k_0__) {
            vars__.push_back(a[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < M; ++k_0__) {
            vars__.push_back(b[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < N; ++k_0__) {
            vars__.push_back(phi[k_0__]);
            }

        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {



            // validate transformed parameters

            // write transformed parameters
            if (include_tparams__) {
            }
            if (!include_gqs__) return;
            // declare and define generated quantities



            // validate generated quantities

            // write generated quantities
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_girtmodel";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= N; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "theta" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= M; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "a" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= M; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= N; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "phi" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }


        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= N; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "theta" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= M; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "a" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= M; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        for (int k_0__ = 1; k_0__ <= N; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "phi" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }


        if (!include_gqs__) return;
    }

}; // model

}

typedef model_girtmodel_namespace::model_girtmodel stan_model;


#endif
