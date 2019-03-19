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

namespace model_irt3plm_namespace {

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
    reader.add_event(0, 0, "start", "model_irt3plm");
    reader.add_event(44, 42, "end", "model_irt3plm");
    return reader;
}

#include <meta_header.hpp>
 class model_irt3plm : public prob_grad {
private:
    int N;
    int M;
    vector<vector<int> > y;
    double D;
    double mu_th;
    double sigma_th;
    double mu_b;
    double sigma_b;
    double location_a;
    double scale_a;
    double alpha_c;
    double beta_c;
    double max_scale;
    double min_scale;
public:
    model_irt3plm(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_irt3plm(stan::io::var_context& context__,
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

        static const char* function__ = "model_irt3plm_namespace::model_irt3plm";
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
            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "mu_th", "double", context__.to_vec());
            mu_th = double(0);
            vals_r__ = context__.vals_r("mu_th");
            pos__ = 0;
            mu_th = vals_r__[pos__++];
            current_statement_begin__ = 9;
            context__.validate_dims("data initialization", "sigma_th", "double", context__.to_vec());
            sigma_th = double(0);
            vals_r__ = context__.vals_r("sigma_th");
            pos__ = 0;
            sigma_th = vals_r__[pos__++];
            current_statement_begin__ = 10;
            context__.validate_dims("data initialization", "mu_b", "double", context__.to_vec());
            mu_b = double(0);
            vals_r__ = context__.vals_r("mu_b");
            pos__ = 0;
            mu_b = vals_r__[pos__++];
            current_statement_begin__ = 11;
            context__.validate_dims("data initialization", "sigma_b", "double", context__.to_vec());
            sigma_b = double(0);
            vals_r__ = context__.vals_r("sigma_b");
            pos__ = 0;
            sigma_b = vals_r__[pos__++];
            current_statement_begin__ = 12;
            context__.validate_dims("data initialization", "location_a", "double", context__.to_vec());
            location_a = double(0);
            vals_r__ = context__.vals_r("location_a");
            pos__ = 0;
            location_a = vals_r__[pos__++];
            current_statement_begin__ = 13;
            context__.validate_dims("data initialization", "scale_a", "double", context__.to_vec());
            scale_a = double(0);
            vals_r__ = context__.vals_r("scale_a");
            pos__ = 0;
            scale_a = vals_r__[pos__++];
            current_statement_begin__ = 14;
            context__.validate_dims("data initialization", "alpha_c", "double", context__.to_vec());
            alpha_c = double(0);
            vals_r__ = context__.vals_r("alpha_c");
            pos__ = 0;
            alpha_c = vals_r__[pos__++];
            current_statement_begin__ = 15;
            context__.validate_dims("data initialization", "beta_c", "double", context__.to_vec());
            beta_c = double(0);
            vals_r__ = context__.vals_r("beta_c");
            pos__ = 0;
            beta_c = vals_r__[pos__++];
            current_statement_begin__ = 16;
            context__.validate_dims("data initialization", "max_scale", "double", context__.to_vec());
            max_scale = double(0);
            vals_r__ = context__.vals_r("max_scale");
            pos__ = 0;
            max_scale = vals_r__[pos__++];
            current_statement_begin__ = 17;
            context__.validate_dims("data initialization", "min_scale", "double", context__.to_vec());
            min_scale = double(0);
            vals_r__ = context__.vals_r("min_scale");
            pos__ = 0;
            min_scale = vals_r__[pos__++];

            // validate, data variables
            current_statement_begin__ = 4;
            check_greater_or_equal(function__,"N",N,1);
            current_statement_begin__ = 5;
            check_greater_or_equal(function__,"M",M,1);
            current_statement_begin__ = 6;
            current_statement_begin__ = 7;
            current_statement_begin__ = 8;
            current_statement_begin__ = 9;
            current_statement_begin__ = 10;
            current_statement_begin__ = 11;
            current_statement_begin__ = 12;
            current_statement_begin__ = 13;
            current_statement_begin__ = 14;
            current_statement_begin__ = 15;
            current_statement_begin__ = 16;
            current_statement_begin__ = 17;
            // initialize data variables


            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 22;
            validate_non_negative_index("theta", "N", N);
            num_params_r__ += N;
            current_statement_begin__ = 23;
            validate_non_negative_index("a", "M", M);
            num_params_r__ += M;
            current_statement_begin__ = 24;
            validate_non_negative_index("b", "M", M);
            num_params_r__ += M;
            current_statement_begin__ = 25;
            validate_non_negative_index("c", "M", M);
            num_params_r__ += M;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_irt3plm() { }


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
            writer__.scalar_lub_unconstrain(min_scale,max_scale,theta[i0__]);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable theta: ") + e.what());
        }

        if (!(context__.contains_r("a")))
            throw std::runtime_error("variable a missing");
        vals_r__ = context__.vals_r("a");
        pos__ = 0U;
        validate_non_negative_index("a", "M", M);
        context__.validate_dims("initialization", "a", "double", context__.to_vec(M));
        std::vector<double> a(M,double(0));
        for (int i0__ = 0U; i0__ < M; ++i0__)
            a[i0__] = vals_r__[pos__++];
        for (int i0__ = 0U; i0__ < M; ++i0__)
            try {
            writer__.scalar_lb_unconstrain(0,a[i0__]);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable a: ") + e.what());
        }

        if (!(context__.contains_r("b")))
            throw std::runtime_error("variable b missing");
        vals_r__ = context__.vals_r("b");
        pos__ = 0U;
        validate_non_negative_index("b", "M", M);
        context__.validate_dims("initialization", "b", "double", context__.to_vec(M));
        std::vector<double> b(M,double(0));
        for (int i0__ = 0U; i0__ < M; ++i0__)
            b[i0__] = vals_r__[pos__++];
        for (int i0__ = 0U; i0__ < M; ++i0__)
            try {
            writer__.scalar_lub_unconstrain(min_scale,max_scale,b[i0__]);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable b: ") + e.what());
        }

        if (!(context__.contains_r("c")))
            throw std::runtime_error("variable c missing");
        vals_r__ = context__.vals_r("c");
        pos__ = 0U;
        validate_non_negative_index("c", "M", M);
        context__.validate_dims("initialization", "c", "double", context__.to_vec(M));
        std::vector<double> c(M,double(0));
        for (int i0__ = 0U; i0__ < M; ++i0__)
            c[i0__] = vals_r__[pos__++];
        for (int i0__ = 0U; i0__ < M; ++i0__)
            try {
            writer__.scalar_lub_unconstrain(0,1,c[i0__]);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable c: ") + e.what());
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
                    theta.push_back(in__.scalar_lub_constrain(min_scale,max_scale,lp__));
                else
                    theta.push_back(in__.scalar_lub_constrain(min_scale,max_scale));
            }

            vector<local_scalar_t__> a;
            size_t dim_a_0__ = M;
            a.reserve(dim_a_0__);
            for (size_t k_0__ = 0; k_0__ < dim_a_0__; ++k_0__) {
                if (jacobian__)
                    a.push_back(in__.scalar_lb_constrain(0,lp__));
                else
                    a.push_back(in__.scalar_lb_constrain(0));
            }

            vector<local_scalar_t__> b;
            size_t dim_b_0__ = M;
            b.reserve(dim_b_0__);
            for (size_t k_0__ = 0; k_0__ < dim_b_0__; ++k_0__) {
                if (jacobian__)
                    b.push_back(in__.scalar_lub_constrain(min_scale,max_scale,lp__));
                else
                    b.push_back(in__.scalar_lub_constrain(min_scale,max_scale));
            }

            vector<local_scalar_t__> c;
            size_t dim_c_0__ = M;
            c.reserve(dim_c_0__);
            for (size_t k_0__ = 0; k_0__ < dim_c_0__; ++k_0__) {
                if (jacobian__)
                    c.push_back(in__.scalar_lub_constrain(0,1,lp__));
                else
                    c.push_back(in__.scalar_lub_constrain(0,1));
            }


            // transformed parameters



            // validate transformed parameters

            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning

            // model body

            current_statement_begin__ = 31;
            lp_accum__.add(cauchy_log<propto__>(a, location_a, scale_a));
            current_statement_begin__ = 32;
            lp_accum__.add(normal_log<propto__>(b, mu_b, sigma_b));
            current_statement_begin__ = 33;
            lp_accum__.add(beta_log<propto__>(c, alpha_c, beta_c));
            current_statement_begin__ = 34;
            lp_accum__.add(normal_log<propto__>(theta, mu_th, sigma_th));
            current_statement_begin__ = 36;
            for (int j = 1; j <= M; ++j) {

                current_statement_begin__ = 37;
                for (int i = 1; i <= N; ++i) {

                    current_statement_begin__ = 38;
                    if (as_bool(logical_eq(get_base1(get_base1(y,i,"y",1),j,"y",2),-(1)))) {
                        current_statement_begin__ = 38;
                        continue;
                    }
                    current_statement_begin__ = 39;
                    lp_accum__.add(bernoulli_log<propto__>(get_base1(get_base1(y,i,"y",1),j,"y",2), (get_base1(c,j,"c",1) + ((1 - get_base1(c,j,"c",1)) * inv_logit(((D * get_base1(a,j,"a",1)) * (get_base1(theta,i,"theta",1) - get_base1(b,j,"b",1))))))));
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
        names__.push_back("c");
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
        dims__.push_back(M);
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
        static const char* function__ = "model_irt3plm_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        vector<double> theta;
        size_t dim_theta_0__ = N;
        for (size_t k_0__ = 0; k_0__ < dim_theta_0__; ++k_0__) {
            theta.push_back(in__.scalar_lub_constrain(min_scale,max_scale));
        }
        vector<double> a;
        size_t dim_a_0__ = M;
        for (size_t k_0__ = 0; k_0__ < dim_a_0__; ++k_0__) {
            a.push_back(in__.scalar_lb_constrain(0));
        }
        vector<double> b;
        size_t dim_b_0__ = M;
        for (size_t k_0__ = 0; k_0__ < dim_b_0__; ++k_0__) {
            b.push_back(in__.scalar_lub_constrain(min_scale,max_scale));
        }
        vector<double> c;
        size_t dim_c_0__ = M;
        for (size_t k_0__ = 0; k_0__ < dim_c_0__; ++k_0__) {
            c.push_back(in__.scalar_lub_constrain(0,1));
        }
            for (int k_0__ = 0; k_0__ < N; ++k_0__) {
            vars__.push_back(theta[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < M; ++k_0__) {
            vars__.push_back(a[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < M; ++k_0__) {
            vars__.push_back(b[k_0__]);
            }
            for (int k_0__ = 0; k_0__ < M; ++k_0__) {
            vars__.push_back(c[k_0__]);
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
        return "model_irt3plm";
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
        for (int k_0__ = 1; k_0__ <= M; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "c" << '.' << k_0__;
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
        for (int k_0__ = 1; k_0__ <= M; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "c" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }


        if (!include_gqs__) return;
    }

}; // model

}

typedef model_irt3plm_namespace::model_irt3plm stan_model;


#endif
